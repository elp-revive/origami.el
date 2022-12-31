;;; origami-parsers.el --- Collection of parsers  -*- lexical-binding: t -*-

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton
;; Copyright (c) 2019-2023 Jen-Chieh Shen

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; Collection of parsers.
;;

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'subr-x)

(require 'dash)
(require 's)

(require 'origami-util)

;;
;; (@* "Exterals" )
;;

(defvar origami-parser-summary-alist)
(declare-function origami-fold-root-node "origami.el")
(declare-function origami-fold-children "origami.el")
(declare-function origami-fold-children-set "origami.el")
(declare-function origami-fold-shallow-merge "origami.el")

;;
;; (@* "Utility" )
;;

(defun origami-get-positions (_content regex &optional predicate fnc-pos)
  "Return a list of positions where REGEX matche in CONTENT.
A position is a cons cell of the character and the numerical position
in the CONTENT.

Optional argument PREDICATE is for filtering.

Optional argument FNC-POS, is function that returns the mark position
from the matching string.  If omitted, the mark will be the first
non-whitespace character of the match."
  (save-excursion
    (goto-char (point-min))
    (let (acc open)
      (while (re-search-forward regex nil t)
        (let ((match (match-string 0)))
          (when (or (null predicate) (funcall predicate (cons match (point))))
            ;; NOTE: Variable `open' is only accurate when `regex' is exactly
            ;; the same. This is only used for symmetric token.
            (setq open (not open))
            (push (cons match
                        (or (ignore-errors (funcall fnc-pos match open))
                            (- (point) (length (string-trim-left match)))))
                  acc))))
      (reverse acc))))

(defun origami-indent-parser (create)
  "Not documented, CREATE."
  (cl-labels
      ((lines (string) (origami-get-positions
                        string ".*?\r?\n" nil
                        (lambda (match &rest _) (- (point) (length match)))))
       (annotate-levels (lines)
                        (-map (lambda (line)
                                ;; TODO: support tabs
                                (let ((indent (length (car (s-match "^ *" (car line)))))
                                      (beg (cdr line))
                                      (end (+ (cdr line) (length (car line)) -1)))
                                  (if (s-blank? (s-trim (car line)))
                                      'newline  ; sentinel representing line break
                                    (vector indent beg end (- end beg)))))
                              lines))
       (indent (line) (if (eq line 'newline) -1 (aref line 0)))
       (beg (line) (aref line 1))
       (end (line) (aref line 2))
       (offset (line) (aref line 3))
       (collapse-same-level (lines)
                            (->>
                             (cdr lines)
                             (-reduce-from (lambda (acc line)
                                             (cond ((and (eq line 'newline) (eq (car acc) 'newline)) acc)
                                                   ((= (indent line) (indent (car acc)))
                                                    (cons (vector (indent (car acc))
                                                                  (beg (car acc))
                                                                  (end line)
                                                                  (offset (car acc)))
                                                          (cdr acc)))
                                                   (t (cons line acc))))
                                           (list (car lines)))
                             (remove 'newline)
                             reverse))
       (create-tree (levels)
                    (if (null levels)
                        levels
                      (let ((curr-indent (indent (car levels))))
                        (->> levels
                             (-partition-by (lambda (l) (= (indent l) curr-indent)))
                             (-partition-all 2)
                             (-mapcat (lambda (x)
                                        ;takes care of multiple identical levels, introduced when there are newlines
                                        (-concat
                                         (-map 'list (butlast (car x)))
                                         (list (cons (-last-item (car x)) (create-tree (cadr x)))))))))))
       (build-nodes (tree)
                    (if (null tree) (cons 0 nil)
                      ;; complexity here is due to having to find the end of the children so that the
                      ;; parent encompasses them
                      (-reduce-r-from
                       (lambda (nodes acc)
                         (cl-destructuring-bind (children-end . children) (build-nodes (cdr nodes))
                           (let ((this-end (max children-end (end (car nodes)))))
                             (cons (max this-end (car acc))
                                   (cons (funcall create
                                                  (beg (car nodes))
                                                  this-end
                                                  (offset (car nodes))
                                                  children)
                                         (cdr acc))))))
                       '(0 . nil)
                       tree))))
    (lambda (content)
      (-> content
          lines
          annotate-levels
          collapse-same-level
          create-tree
          build-nodes
          cdr))))

(defun origami-build-pair-tree (create open close else positions &optional fnc-offset)
  "Build the node tree from POSITIONS.

Argument CREATE is the parser function to create the nodes.

Arguments OPEN, CLOSE and ELSE are regular expressions used to determine the
type of a position.  An OPEN match will start a new pending node, a CLOSE match
will finish a started pending node, an ELSE match works like CLOSE + OPEN - it
will finish a started pending node, then immediately start a new one.

Argument POSITIONS is a list of cons cell form by (match . point).

Optional argument FNC-OFFSET is a function that return's the position of the
node offset.  This point is the actual position that folds (hide) the code
in region; the beginning position will be the header that could triggers the
fold/unfold action."
  (cl-labels
      ((build (positions)
              ;; recursive function to build nodes from positions, returns cons cell with
              ;; (remaining-positions . created-nodes)
              (let (;; nil indicates the current tree level was finished by
                    ;; a closing match, and we need to ascend back to upper
                    ;; level from recursion
                    (tree-level-unfinished t)
                    acc        ; accumulates created nodes
                    beg-pos    ; beginning pos of started, but unfinished (not created) node
                    beg-match  ; beginning match of started, but unfinished node
                    cur-pos    ; pos of current position
                    cur-match)
                (while (and tree-level-unfinished positions)
                  (setq cur-match (caar positions)
                        cur-pos (cdar positions))
                  (origami-log "\f")
                  (origami-log "current match: %s" cur-match)
                  (origami-log "beg (pos, cur-match): %s, %s" beg-pos beg-match)
                  (cond
                     ;;; --- ELSE --------------------------------------------
                   ((and (stringp else) (string-match-p else cur-match))
                    (origami-log ">> else: " cur-match)
                    ;; Stay on same level - finish beg node & start new one. As we are starting a
                    ;; new node at cur-pos, make the beg node end at one char before that.
                    (push (funcall create beg-pos (1- cur-pos)
                                   (or (origami-util-function-offset fnc-offset beg-pos beg-match)
                                       (length beg-match))
                                   nil)
                          acc)
                    ;; begin a new pair
                    (setq beg-pos cur-pos
                          beg-match cur-match
                          positions (cdr positions)))
                     ;;; --- OPEN --------------------------------------------
                   ((string-match-p open cur-match)
                    (origami-log ">> open: " cur-match)
                    (if beg-pos  ; go down a level
                        (progn
                          (origami-log "dig in...")
                          (let* ((res (build positions)) ; recurse
                                 (new-pos (car res))
                                 (children (cdr res))
                                 ;; auto-close unclosed folds at point-max
                                 (close-pos (or (cdar new-pos) (point-max)))
                                 (node (funcall create beg-pos close-pos
                                                (or (origami-util-function-offset fnc-offset beg-pos beg-match)
                                                    (length beg-match))
                                                children)))
                            ;; close with children
                            (when node (push node acc))
                            (setq beg-pos nil
                                  beg-match nil
                                  positions (cdr new-pos))))
                      ;; begin a new pair
                      (setq beg-pos cur-pos
                            beg-match cur-match
                            positions (cdr positions))))
                     ;;; --- CLOSE --------------------------------------------
                   ((string-match-p close cur-match)
                    (origami-log ">> close: " cur-match)
                    (if beg-pos  ; close with no children
                        (progn
                          (let ((node (funcall create beg-pos cur-pos
                                               (or (origami-util-function-offset fnc-offset beg-pos beg-match)
                                                   (length beg-match))
                                               nil)))
                            (when node (push node acc)))
                          (setq beg-pos nil
                                beg-match nil
                                positions (cdr positions)))
                      ;; stop loop to reascend to upper tree level
                      (setq tree-level-unfinished nil)))))
                (origami-log "dig out...")
                ;; Pass remaining positions and accumulated nodes up the recursion stack
                (cons positions (reverse acc)))))
    (cdr (build positions))))

(defun origami--force-pair-positions (positions)
  "Force POSITIONS pair into a length of even number."
  (let ((last-pos-symbol "") result)
    (-filter (lambda (position)
               (setq result (not (string= last-pos-symbol (car position)))
                     last-pos-symbol (car position))
               result)
             positions)))

(defvar origami--strict-pair nil
  "Strictly check the pair tree must have length of even number.

This flag is use to debug function `origami-build-pair-tree-2'.")

(defun origami-build-pair-tree-2 (create positions &optional fnc-offset)
  "Build pair list tree.

POSITIONS is from by a list of cons cell form by (syntax . point).
Notice POSITIONS' length must be an even number due to the list must be
pair up.  For instance,

  <0> (syntax . point),
  <1> (syntax . point),
  <2> (syntax . point),
  <3> (syntax . point), ...

Item N and the next item (N + 1) should be a pair; hence, N should always
be even number (if count starting from 0 and not 1).

Optional argument FNC-OFFSET is a function that return's the position of
the offset."
  (let ((index 0) (len (length positions))
        beg end offset pos-beg pos-end match
        ov ovs)
    (when (origami-util-is-odd len)
      (if origami--strict-pair
          (error "Pair tree 2 should not have length of odd number: %s" len)
        (setq positions (origami--force-pair-positions positions))))
    (while (< index len)
      (setq pos-beg (nth index positions)
            pos-end (nth (1+ index) positions)
            beg (cdr pos-beg) end (cdr pos-end)
            offset (length (string-trim (car pos-beg)))
            match (car pos-beg)
            ov (ignore-errors (funcall create beg end
                                       (or (origami-util-function-offset fnc-offset beg match)
                                           offset)
                                       nil)))
      (when ov (push ov ovs))
      (cl-incf index 2))
    (reverse ovs)))

(defun origami-build-pair-tree-single (create syntax &optional predicate fnc-pos fnc-offset)
  "Build pair tree for single line SYNTAX.

This is use for syntax continuous appears repeatedly on each line.
For instance,

  1 | # This is comment line.
  2 | #
  3 | # This is also a comment line.
  4 |
  5 | # Another comment line.

In the above case, L1 - L3 will be mark; but L5 will be ignored.  This
function can be use for any kind of syntax like `//`, `;`, `#`."
  (lambda (content)
    (let* ((positions (origami-get-positions content syntax predicate fnc-pos))
           valid-positions)
      (let ((index 0) (len (length positions))
            last-position position
            line pos last-line start-p
            on-next-line-p)
        (while (< index len)
          (setq position (nth index positions)
                pos (cdr position)
                line (line-number-at-pos pos t)
                on-next-line-p (when last-line (= 1 (- line last-line))))
          (if (= index 0)
              (setq last-position position
                    last-line line)
            (if start-p
                ;; Collect ending point; then wrap up.
                (unless on-next-line-p
                  (setcdr last-position (origami-util-pos-line-end (cdr last-position)))
                  (push last-position valid-positions)
                  (setq start-p nil))
              ;; Collect starting point.
              (when on-next-line-p
                (push last-position valid-positions)
                (setq start-p t)))
            (setq last-position position
                  last-line line))
          (cl-incf index))
        ;; NOTE: Add the last from list `positions' in order to pair up!
        (when start-p
          (setcdr last-position (origami-util-pos-line-end (cdr last-position)))
          (push last-position valid-positions)))
      (setq valid-positions (reverse valid-positions))
      (origami-build-pair-tree-2 create valid-positions fnc-offset))))

;;
;; (@* "Token Identification" )
;;

(defvar origami-doc-faces
  '(font-lock-doc-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    tree-sitter-hl-face:comment
    tree-sitter-hl-face:doc
    hl-todo)
  "List of face that apply for document string.")

(defun origami-doc-faces-p (obj &optional trim)
  "Return non-nil if face at OBJ is within `origami-doc-faces' list.

Optional argument TRIM, see function `origami-util-get-face'."
  (origami-util-is-face obj origami-doc-faces trim))

(defun origami-filter-doc-face (position)
  "Predicate we use to filter out for non-comment.

Argument POSITION can either be cons (match . position); or a integer value."
  (if (consp position)
      (or (origami-doc-faces-p (car position) t)
          (origami-util-comment-block-p (cdr position)))
    (not (origami-filter-code-face position))))

(defun origami-filter-code-face (position)
  "Predicate we use to filter out non-code.

Argument POSITION can either be cons (match . position); or a integer value."
  (when (consp position) (setq position (cdr position)))
  (not (origami-util-comment-or-string-p position)))

(defun origami-search-forward (sym predicate &optional start bound)
  "Find SYM and return it's position.

Argument PREDICATE is use to test for valid condition.  Optional argument
START is the starting point to scan the symbol.  Optional argument BOUND
is the ending point to stop the scanning processs."
  (unless bound (setq bound (line-end-position)))
  (save-excursion
    (when start (goto-char start))
    (let ((pt (point)))
      (while (and (re-search-forward sym bound t)
                  (ignore-errors (funcall predicate (point))))
        (setq pt (point)))
      pt)))

;;
;; (@* "Parsers" )
;;

(defun origami-parsers--prefix (create prefix)
  "Internal single line syntax parser with PREFIX."
  (origami-build-pair-tree-single
   create prefix 'origami-filter-doc-face
   (lambda (&rest _) (line-beginning-position))
   (lambda (&rest _) (origami-search-forward prefix 'origami-filter-doc-face))))

(defun origami-parser-triple-slash (create)
  "Parser for single line syntax triple slash."
  (origami-parsers--prefix create "^[ \t\r\n]*///"))

(defun origami-parser-double-slash (create)
  "Parser for single line syntax double slash."
  (origami-parsers--prefix create "^[ \t]*//"))

(defun origami-parser-single-sharp (create)
  "Parser for single line syntax single sharp."
  (origami-parsers--prefix create "^[ \t]*#"))

(defun origami-parser-double-semi-colon (create)
  "Parser for single line syntax double semi-colon."
  (origami-parsers--prefix create "^[ \t]*;;"))

(defun origami-parser-double-dash (create)
  "Parser for single line syntax double dash."
  (origami-parsers--prefix create "^[ \t]*--"))

(defun origami-parser-double-colon (create)
  "Parser for single line syntax double colon."
  (origami-parsers--prefix create "^[ \t]*::"))

(defun origami-parser-rem (create)
  "Parser for single line syntax REM."
  (origami-parsers--prefix create "^[ \t]*[Rr][Ee][Mm]"))

;; TODO: tag these nodes? have ability to manipulate nodes that are tagged?
;; in a scoped fashion?
(defun origami-javadoc-parser (create)
  "Parser for Javadoc."
  (lambda (content)
    (let* ((beg "/[*]")
           (positions
            (origami-get-positions
             content "/\\*\\|\\*/"
             (lambda (pos &rest _) (origami-filter-doc-face pos))
             (lambda (match &rest _)
               (when (string-match-p beg match) (line-beginning-position))))))
      (origami-build-pair-tree-2
       create positions
       (lambda (&rest _) (origami-search-forward beg #'origami-filter-doc-face))))))

(defun origami-python-doc-parser (create)
  "Parser for Python document string."
  (lambda (content)
    (let* ((sec "\"\"\"")
           (positions
            (origami-get-positions
             content sec
             (lambda (pos &rest _) (origami-filter-doc-face pos))
             (lambda (_match open &rest _) (when open (line-beginning-position))))))
      (origami-build-pair-tree-2
       create positions
       (lambda (&rest _)
         (origami-search-forward sec #'origami-filter-doc-face))))))

(defun origami-lua-doc-parser (create)
  "Parser for Lua document string."
  (lambda (content)
    (let* ((beg '("--[[][[]")) (end '("]]"))
           (_beg-regex (origami-util-comment-regex beg))
           (_end-regex (origami-util-comment-regex end))
           (all-regex (origami-util-comment-regex (append beg end)))
           (positions
            (origami-get-positions
             content all-regex
             (lambda (pos &rest _) (origami-filter-doc-face pos))
             (lambda (match &rest _)
               (when (string-match-p beg match) (line-beginning-position))))))
      (origami-build-pair-tree-2
       create positions
       (lambda (&rest _)
         (origami-search-forward beg #'origami-filter-doc-face))))))

(defun origami-batch-parser (create)
  "Parser for Batch."
  (let ((p-rem (origami-parser-rem create))
        (p-dc (origami-parser-double-colon create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall p-rem content))
                                   (origami-fold-root-node (funcall p-dc content)))))))

(defun origami-c-style-parser (create)
  "Parser for C style programming language."
  (lambda (content)
    (let ((positions
           (origami-get-positions
            content "[{}]"
            (lambda (pos &rest _) (origami-filter-code-face pos))
            (lambda (match &rest _)
              (when (string= match "{")
                (origami-search-forward "}" #'origami-filter-code-face
                                        (line-beginning-position) (point)))))))
      (origami-build-pair-tree
       create "{" "}" nil positions
       (lambda (&rest _)
         (origami-search-forward "{" #'origami-filter-code-face))))))

(defun origami--macro-regex (direc)
  "Return a regular expression to search for DIREC."
  (format "[ \t]*#[ \t]*%s" direc))

(defun origami-c-macro-parser (create)
  "Parser for C style macro."
  (lambda (content)
    (let* ((beg `(,(origami--macro-regex "if[n]*[d]*[e]*[f]*")))
           (end `(,(origami--macro-regex "endif")))
           (else `(,(origami--macro-regex "else") ,(origami--macro-regex "elif")))
           (beg-regex (origami-util-keywords-regex beg))
           (end-regex (origami-util-keywords-regex end))
           (else-regex (origami-util-keywords-regex else))
           (all-regex (origami-util-keywords-regex (append beg end else)))
           (positions
            (origami-get-positions
             content all-regex
             (lambda (pos &rest _) (origami-filter-code-face pos))
             (lambda (match &rest _)
               (if (origami-util-contain-list-type-str end match 'regex)
                   ;; keep end pos on separate line on folding
                   (1- (line-beginning-position))
                 (line-beginning-position))))))
      (origami-build-pair-tree create beg-regex end-regex else-regex
                               positions
                               (lambda (&rest _) (line-end-position))))))

(defun origami-csharp-macro-parser (create)
  "Parser for C# style macro."
  (lambda (content)
    (let* ((beg `(,(origami--macro-regex "if[n]*[d]*[e]*[f]*")
                  ,(origami--macro-regex "region")))
           (end `(,(origami--macro-regex "endif") ,(origami--macro-regex "endregion")))
           (else `(,(origami--macro-regex "else") ,(origami--macro-regex "elif")))
           (beg-regex (origami-util-keywords-regex beg))
           (end-regex (origami-util-keywords-regex end))
           (else-regex (origami-util-keywords-regex else))
           (all-regex (origami-util-keywords-regex (append beg end else)))
           (positions
            (origami-get-positions
             content all-regex
             (lambda (pos &rest _) (origami-filter-code-face pos))
             (lambda (match &rest _)
               (if (origami-util-contain-list-type-str end match 'regex)
                   ;; keep end pos on separate line on folding
                   (1- (line-beginning-position))
                 (line-beginning-position))))))
      (origami-build-pair-tree create beg-regex end-regex else-regex
                               positions
                               (lambda (&rest _) (line-end-position))))))

(defun origami-c-parser (create)
  "Parser for C."
  (let ((c-style (origami-c-style-parser create))
        (macros (origami-c-macro-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall macros content))
                                   (origami-fold-root-node (funcall javadoc content)))))))

(defun origami-c++-parser (create)
  "Parser for C++."
  (origami-c-parser create))

(defun origami-objc-parser (create)
  "Parser for Objective-C."
  (origami-c-parser create))

(defun origami-java-parser (create)
  "Parser for Java."
  (let ((c-style (origami-c-style-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall javadoc content)))))))

(defun origami-js-parser (create)
  "Parser for JavaScript."
  (origami-java-parser create))

(defun origami-csharp-parser (create)
  "Parser for C#."
  (let ((c-style (origami-c-style-parser create))
        (macros (origami-csharp-macro-parser create))
        (javadoc (origami-javadoc-parser create))
        (p-ts (origami-parser-triple-slash create))
        (p-ds (origami-parser-double-slash create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall macros content))
                                   (origami-fold-root-node (funcall javadoc content))
                                   (origami-fold-root-node (funcall p-ts content))
                                   (origami-fold-root-node (funcall p-ds content)))))))

(defun origami-python-subparser (create beg end)
  "Find all fold block between BEG and END.
See function `origami-python-parser' description for argument CREATE."
  (goto-char beg)
  (let (acc)
    ;; iterate all same level children.
    (while (and (beginning-of-defun -1) (<= (point) end))  ; have children between beg and end?
      (let* ((new-beg (point))
             (new-offset (progn (search-forward-regexp ":" nil t) (- (point) new-beg)))
             (new-end (progn (end-of-defun) (point))))
        (setq acc (cons (funcall create new-beg new-end new-offset
                                 (origami-python-subparser create new-beg new-end))
                        acc))
        (goto-char new-end)))
    acc))

(defun origam-python-parser-indent (create)
  "Indent Python core parser."
  (lambda (content)
    (with-temp-buffer
      (insert content)
      ;; Prevent infinite recursive
      (let (prog-mode-hook after-change-major-mode-hook) (python-mode))
      (origami-python-subparser create (point-min) (point-max)))))

(defun origami-python-parser (create)
  "Parser for Python."
  (let ((py-indent (origam-python-parser-indent create))
        (python-doc (origami-python-doc-parser create))
        (p-ss (origami-parser-single-sharp create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall py-indent content))
                                   (origami-fold-root-node (funcall python-doc content))
                                   (origami-fold-root-node (funcall p-ss content)))))))

(defun origami-lisp-parser (create regex)
  "Parser for Lisp."
  (lambda (content)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (beginning-of-defun -1)
      (let (beg end offset acc)
        (while (< (point) (point-max))
          (setq beg (point))
          (search-forward-regexp regex nil t)
          (setq offset (- (point) beg))
          (end-of-defun)
          (backward-char)  ; move point to one after the last paren
          (setq end (1- (point)))  ; don't include the last paren in the fold
          (when (> offset 0)
            (push (funcall create beg end offset nil) acc))
          (beginning-of-defun -1))
        (reverse acc)))))

(defun origami-elisp-parser (create)
  "Parser for Emacs Lisp."
  (origami-lisp-parser create "(def\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun origami-go-parser (create)
  "Parser for Go."
  (let ((c-style (origami-c-style-parser create))
        (p-ds (origami-parser-double-slash create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall p-ds content)))))))

(defun origami-lua-core-parser (create)
  "Core parser for Lua."
  (lambda (content)
    (let* ((beg '("function" "then" "do"))
           (end '("end" "elseif"))
           (else '("else"))
           (beg-regex (origami-util-keywords-regex beg))
           (end-regex (origami-util-keywords-regex end))
           (else-regex (origami-util-keywords-regex else))
           (all-regex (origami-util-keywords-regex (append beg end else)))
           (last-beg-pt -1) (current-beg-pt -1)
           overlaps-pts
           (positions
            (origami-get-positions
             content all-regex
             (lambda (pos &rest _) (origami-filter-code-face pos))
             (lambda (match &rest _)
               (setq current-beg-pt (line-beginning-position))
               (if (origami-util-contain-list-type-str end match 'strict)
                   (if (= last-beg-pt current-beg-pt)
                       (progn
                         (push current-beg-pt overlaps-pts)
                         (- (point) (length match)))
                     ;; keep end pos on separate line on folding
                     (1- current-beg-pt))
                 (setq last-beg-pt current-beg-pt)
                 current-beg-pt)))))
      (origami-build-pair-tree create beg-regex end-regex else-regex
                               positions
                               (lambda (match &rest _)
                                 (if (memq (point) overlaps-pts)
                                     (progn
                                       ;; default to after point match
                                       (search-forward match nil t)
                                       (point))
                                   (if (string= "function" match)
                                       (origami-search-forward ")" #'origami-filter-code-face)
                                     (line-end-position))))))))

(defun origami-lua-parser (create)
  "Parser for Lua."
  (let ((p-lua (origami-lua-core-parser create))
        (p-lua-doc (origami-lua-doc-parser create))
        (p-dd (origami-parser-double-dash create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall p-lua content))
                                   (origami-fold-root-node (funcall p-lua-doc content))
                                   (origami-fold-root-node (funcall p-dd content)))))))

(defun origami-ruby-core-parser (create)
  "Core parser for Ruby."
  (lambda (content)
    (let* ((beg '("def" "class" "module" "if" "unless" "while" "until" "case" "for" "begin"))
           (end '("end"))
           (else '("else" "when" "elsif"))
           (beg-regex (origami-util-keywords-regex beg))
           (end-regex (origami-util-keywords-regex end))
           (else-regex (origami-util-keywords-regex else))
           (all-regex (origami-util-keywords-regex (append beg end else)))
           (last-beg-pt -1) (current-beg-pt -1)
           overlaps-pts
           (positions
            (origami-get-positions
             content all-regex
             (lambda (pos &rest _) (origami-filter-code-face pos))
             (lambda (match &rest _)
               (setq current-beg-pt (line-beginning-position))
               (if (origami-util-contain-list-type-str end match 'strict)
                   (if (= last-beg-pt current-beg-pt)
                       (progn
                         (push current-beg-pt overlaps-pts)
                         (- (point) (length match)))
                     ;; keep end pos on separate line on folding
                     (1- current-beg-pt))
                 (setq last-beg-pt current-beg-pt)
                 current-beg-pt)))))
      (origami-build-pair-tree create beg-regex end-regex else-regex
                               positions
                               (lambda (match &rest _)
                                 (if (memq (point) overlaps-pts)
                                     (progn
                                       ;; default to after point match
                                       (search-forward match nil t)
                                       (point))
                                   (line-end-position)))))))

(defun origami-ruby-parser (create)
  "Parser for Ruby."
  (let ((p-ruby (origami-ruby-core-parser create))
        (p-ss (origami-parser-single-sharp create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall p-ruby content))
                                   (origami-fold-root-node (funcall p-ss content)))))))

(defun origami-rust-parser (create)
  "Parser for Rust."
  (origami-csharp-parser create))

(defun origami-clj-parser (create)
  "Parser for Clojure."
  (origami-lisp-parser create "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(defun origami-scala-parser (create)
  "Parser for Scala."
  (let ((c-style (origami-c-style-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall javadoc content)))))))

(defun origami-sh-parser (create)
  "Parser for Shell script."
  (origami-parser-single-sharp create))

(defun origami-swift-parser (create)
  "Parser for Swift."
  (origami-c-parser create))

(defun origami-markers-parser (start-marker end-marker &optional is-regex)
  "Create a parser for simple start and end markers.

If IS-REGEX is t, the markers will be interpreted as regular
expressions."
  (let ((rx-beg (if is-regex start-marker
                  (regexp-quote start-marker)))
        (rx-end (if is-regex end-marker
                  (regexp-quote end-marker)))
        (rx-all (rx-to-string
                 (if is-regex
                     `(or (regexp ,start-marker) (regexp ,end-marker))
                   `(or ,start-marker ,end-marker)))))
    (lambda (create)
      (lambda (content)
        (let ((positions (origami-get-positions content rx-all)))
          (origami-build-pair-tree create rx-beg rx-end nil positions))))))

(defun origami-markdown-parser (create)
  "Parser for Markdown."
  (lambda (content)
    (let* ((sec "```")
           (positions (origami-get-positions
                       content sec nil
                       (lambda (&rest _)
                         (1- (line-beginning-position))))))
      (origami-build-pair-tree-2
       create positions
       (lambda (match &rest _) (+ (point) (length match) 1))))))

(defun origami-org-parser (create)
  "Parser for Org."
  (lambda (content)
    (let* ((beg '("#[+]BEGIN_SRC" "#[+]BEGIN_EXAMPLE"))
           (end '("#[+]END_SRC" "#[+]END_EXAMPLE"))
           (beg-regex (origami-util-keywords-regex beg))
           (end-regex (origami-util-keywords-regex end))
           (all-regex (origami-util-keywords-regex (append beg end)))
           (positions (origami-get-positions
                       content all-regex nil
                       (lambda (&rest _) (1- (line-beginning-position))))))
      (origami-build-pair-tree create beg-regex end-regex nil
                               positions
                               (lambda (match &rest _)
                                 (+ (point) (length match) 1))))))

(defun origami-xml-base-parser (create &optional remove-leaves ignored-tags-regex)
  "Base parser for xml style markup."
  (cl-labels
      ((valid-pos-p (pos)
                    (and  (origami-filter-code-face pos)
                          (or (null ignored-tags-regex)
                              (not (string-match-p ignored-tags-regex (car pos))))))
       (build-nodes (content)
                    (rx-let
                        ((beg-tag
                          (seq "<"
                               ;; elements start with letter or _, don't match preamble
                               (any word "_")
                               (zero-or-more
                                (or
                                 ;; anything but closing tag or attribute
                                 (not (any ">"
                                           ;; ignore self-closing tags
                                           "/"
                                           ;; attribute values require their own matching
                                           "\""))
                                 ;; attribute value
                                 (seq "\""
                                      (zero-or-more (not "\""))
                                      "\"")))
                               ">"))
                         (end-tag
                          (seq "</" (any word "_") (zero-or-more (not ">")) ">")))
                      ;; no need to care for comments/CDATA, these pos are filtered by face
                      ;; in valid-pos-p
                      (let* ((rx-beg-tag (rx beg-tag))
                             (rx-end-tag (rx end-tag))
                             (rx-all (rx (or beg-tag end-tag)))
                             (positions (origami-get-positions content rx-all #'valid-pos-p)))
                        (origami-build-pair-tree create rx-beg-tag rx-end-tag nil positions))))
       (trim (nodes)
             ;; trims leaves, if required
             ;; no artificial root node yet, so base level is a list of nodes
             (if (not remove-leaves)
                 nodes
               (let ((trimmed-nodes
                      ;; remove base level leaves
                      (seq-remove (lambda (node) (null (origami-fold-children node)))
                                  nodes)))
                 (if (seq-empty-p trimmed-nodes)
                     trimmed-nodes
                   ;; recurse to remove deeper level leaves
                   (-map #'trim-recurse trimmed-nodes)))))
       (trim-recurse (tree)
                     (if (origami-fold-children tree)
                         ;; has children - recurse for children, and don't remove this node
                         (let ((trimmed-childs
                                (-map #'trim-recurse (origami-fold-children tree))))
                           ;; return copy of node with filtered childs
                           (origami-fold-children-set tree
                                                      (seq-remove #'null trimmed-childs)))
                       ;; no childrens, remove this node
                       nil)))
    (lambda (content)
      (-some-> content
        build-nodes
        trim))))

(defcustom origami-xml-skip-leaf-nodes t
  "In xml files, only elements with child elements will be
foldable, not if they contain text only."
  :type 'boolean
  :group 'origami)

(defun origami-xml-parser (create)
  "Parser for xml."
  (origami-xml-base-parser create origami-xml-skip-leaf-nodes))

(defun origami-html-parser (create)
  "Parser for html."
  (rx-let ((ignore-tags (&rest tags) (seq "<" (or tags) word-end)))
    ;; Self-closing tags (void elements) without closing slash would throw off parser, ignore
    (let ((ignore-tags-rx
           (rx (ignore-tags "area" "base" "br" "col" "command" "embed" "hr" "img" "input"
                            "keygen" "link" "menuitem" "meta" "param" "source" "track" "wbr"))))
      (origami-xml-base-parser create nil ignore-tags-rx))))

(defun origami-json-parser (create)
  "Parser for JSON."
  (lambda (content)
    (let* ((rx-beg "[[{]")
           (rx-end "[]}]")
           (rx-all "[][{}]")
           (positions
            (origami-get-positions content rx-all
                                   (lambda (pos &rest _) (origami-filter-code-face pos)))))
      (origami-build-pair-tree
       create rx-beg rx-end nil positions
       (lambda (&rest _)
         (origami-search-forward rx-beg #'origami-filter-code-face))))))

(defcustom origami-parser-alist
  `((actionscript-mode     . origami-java-parser)
    (bat-mode              . origami-batch-parser)
    (c-mode                . origami-c-parser)
    (c++-mode              . origami-c++-parser)
    (clojure-mode          . origami-clj-parser)
    (cperl-mode            . origami-c-style-parser)
    (csharp-mode           . origami-csharp-parser)
    (dart-mode             . origami-c-style-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (go-mode               . origami-go-parser)
    (html-mode             . origami-html-parser)
    (java-mode             . origami-java-parser)
    (javascript-mode       . origami-js-parser)
    (js-mode               . origami-js-parser)
    (js2-mode              . origami-js-parser)
    (js3-mode              . origami-js-parser)
    (json-mode             . origami-json-parser)
    (kotlin-mode           . origami-java-parser)
    (lisp-mode             . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (lua-mode              . origami-lua-parser)
    (markdown-mode         . origami-markdown-parser)
    (nxml-mode             . origami-xml-parser)
    (objc-mode             . origami-objc-parser)
    (org-mode              . origami-org-parser)
    (perl-mode             . origami-c-style-parser)
    (php-mode              . origami-java-parser)
    (python-mode           . origami-python-parser)
    (rjsx-mode             . origami-js-parser)
    (ruby-mode             . origami-ruby-parser)
    (rust-mode             . origami-rust-parser)
    (scala-mode            . origami-scala-parser)
    (sh-mode               . origami-sh-parser)
    (swift-mode            . origami-swift-parser)
    (triple-braces         . ,(origami-markers-parser "{{{" "}}}"))
    (typescript-mode       . origami-js-parser)
    (web-mode              . origami-html-parser))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

;;
;; (@* "Summary" )
;;

(defcustom origami-show-summary t
  "Flag to show summary if available."
  :type 'boolean
  :group 'origami)

(defcustom origami-max-summary-length 60
  "Maximum length for summary to display."
  :type '(choice (const :tag "nil" nil)
                 (integer :tag "positive integer number"))
  :group 'origami)

(defcustom origami-summary-exceeded-string "..."
  "String that added after display summary.

This happens only when summary length is larger than variable
`origami-max-summary-length'."
  :type 'string
  :group 'origami)

(defcustom origami-summary-format " <S> %s "
  "Prefix string added before summary overlay."
  :type 'string
  :group 'origami)

(defun origami-valid-content-p (content)
  "Return non-nil if CONTENT is a valid document string for extraction.

Some programmers use some type of characters for splitting the code module
into sections.  For instance, ===, ---, ///, =-=, etc.  Try to omit these
type of content by checking the word boundary's existence."
  (string-match-p "\\w" content))

(defun origami--apply-sym (line sym)
  "Remove SYM from LINE."
  (when (string-prefix-p sym line)
    (setq line (substring line (length sym) (length line))
          line (string-trim line)))
  line)

(defun origami-extract-summary (doc-str sym)
  "Extract only document summary from DOC-STR using SYM"
  (let ((lines (split-string doc-str "\n")) new-lines)
    (dolist (line lines)
      (setq line (string-trim line))
      (cond ((listp sym)
             (dolist (c sym) (setq line (origami--apply-sym line c))))
            (t (setq line (origami--apply-sym line sym))))
      (when (origami-valid-content-p line) (push line new-lines)))
    (reverse new-lines)))

(defun origami-doc-extract-summary (doc-str sym)
  "Default way to extract the doc summary from DOC-STR."
  (let* ((lines (origami-extract-summary doc-str sym)) (summary (nth 0 lines)))
    (when summary (setq summary (string-trim summary)))
    (if (string-empty-p summary) nil summary)))

(defun origami--generic-summary (doc-str sym)
  "Generic DOC-STR extraction using SYM."
  (when (origami-doc-faces-p doc-str)
    (origami-doc-extract-summary doc-str sym)))

(defun origami-batch-summary (doc-str)
  "Extract batch summary from DOC-STR."
  (origami--generic-summary doc-str '("::" "rem" "REM")))

(defun origami-csharp-vsdoc-summary (doc-str)
  "Extract C# vsdoc summary from DOC-STR."
  (let ((type-triple (string-match-p "///" doc-str)))
    (setq doc-str (s-replace-regexp "<[/]*[^>]+." "" doc-str))
    (origami--generic-summary doc-str (if type-triple "///" "//"))))

(defun origami-javadoc-summary (doc-str)
  "Extract javadoc summary from DOC-STR."
  (origami--generic-summary doc-str "*"))

(defun origami-go-summary (doc-str)
  "Extract Go document summary from DOC-STR."
  (origami--generic-summary doc-str "//"))

(defun origami-lua-doc-summary (doc-str)
  "Extract Lua document string from DOC-STR."
  (origami--generic-summary doc-str "--"))

(defun origami-python-doc-summary (doc-str)
  "Extract Python document string from DOC-STR."
  (origami--generic-summary doc-str "\"\"\""))

(defun origami-ruby-doc-summary (doc-str)
  "Extract Ruby document string from DOC-STR."
  (origami--generic-summary doc-str "#"))

(defun origami-rust-doc-summary (doc-str)
  "Extract Rust document summary from DOC-STR."
  (origami--generic-summary doc-str "///"))

(defun origami-c-macro-summary (doc-str)
  "Parse C macro summary from DOC-STR."
  (when (origami-util-is-face doc-str '(font-lock-preprocessor-face
                                        preproc-font-lock-preprocessor-background))
    (origami-doc-extract-summary doc-str "")))

(defun origami-c-summary (doc-str)
  "Summary parser for C from DOC-STR."
  (or (origami-javadoc-summary doc-str)
      (origami-c-macro-summary doc-str)))

(defun origami-markdown-summary (doc-str)
  "Extract Makrdown block from DOC-STR."
  (origami-doc-extract-summary doc-str '()))

(defun origami-org-summary (doc-str)
  "Extract Org block from DOC-STR."
  (origami-doc-extract-summary doc-str '()))

(defun origami-get-summary-parser ()
  "Return the summary parser from `origami-parser-summary-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) origami-parser-summary-alist))

(defun origami--keep-summary-length (summary)
  "Keep the SUMMARY length to `origami-max-summary-length'."
  (let ((len-sum (length summary))
        (len-exc (length origami-summary-exceeded-string)))
    (when (< origami-max-summary-length len-sum)
      (setq summary (substring summary 0 (- origami-max-summary-length len-exc))
            summary (concat summary origami-summary-exceeded-string))))
  summary)

(defun origami-summary-apply-format (summary)
  "Return the SUMMARY that has added the summary prefix."
  (format origami-summary-format summary))

(defun origami-get-summary (doc-str)
  "Extract summary from DOC-STR in order to display ontop of the overlay."
  (let ((parser (cdr (origami-get-summary-parser))) summary)
    (when parser
      (setq summary (funcall parser doc-str))
      (when (integerp origami-max-summary-length)
        (setq summary (origami--keep-summary-length summary)))
      (when summary
        (setq summary (origami-summary-apply-format summary)
              summary (propertize summary 'face 'origami-fold-replacement-face))))
    summary))

(defcustom origami-parser-summary-alist
  `((actionscript-mode . origami-javadoc-summary)
    (bat-mode          . origami-batch-summary)
    (c-mode            . origami-c-summary)
    (c++-mode          . origami-c-summary)
    (csharp-mode       . origami-csharp-vsdoc-summary)
    (go-mode           . origami-go-summary)
    (java-mode         . origami-javadoc-summary)
    (javascript-mode   . origami-javadoc-summary)
    (js-mode           . origami-javadoc-summary)
    (js2-mode          . origami-javadoc-summary)
    (js3-mode          . origami-javadoc-summary)
    (kotlin-mode       . origami-javadoc-summary)
    (lua-mode          . origami-lua-doc-summary)
    (markdown-mode     . origami-markdown-summary)
    (objc-mode         . origami-c-summary)
    (org-mode          . origami-org-summary)
    (php-mode          . origami-javadoc-summary)
    (python-mode       . origami-python-doc-summary)
    (rjsx-mode         . origami-javadoc-summary)
    (ruby-mode         . origami-ruby-doc-summary)
    (rust-mode         . origami-rust-doc-summary)
    (scala-mode        . origami-javadoc-summary)
    (sh-mode           . origami-javadoc-summary)
    (swift-mode        . origami-c-summary)
    (typescript-mode   . origami-javadoc-summary))
  "Alist mapping major-mode to doc parser function."
  :type 'hook
  :group 'origami)

(provide 'origami-parsers)
;;; origami-parsers.el ends here
