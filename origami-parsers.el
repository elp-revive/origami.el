;;; origami-parsers.el --- Collection of parsers  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Version: 2.1
;; Keywords: parsers
;; URL: https://github.com/jcs-elpa/origami.el

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton
;; Copyright (c) 2019-2020 Jen-Chieh Shen

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'subr-x)

;;
;; (@* "Utility" )
;;

(defun origami-get-positions (content regex)
  "Return a list of positions where REGEX matche in CONTENT.
A position is a cons cell of the character and the numerical position
in the CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward regex nil t)
        (let ((match (match-string 0)))
          (setq acc (cons (cons match (- (point) (length match)))
                          acc))))
      (reverse acc))))

(defun origami-indent-parser (create)
  "Not documented, CREATE."
  (cl-labels
      ((lines (string) (origami-get-positions string ".*?\r?\n"))
       (annotate-levels (lines)
                        (-map (lambda (line)
                                ;; TODO: support tabs
                                (let ((indent (length (car (s-match "^ *" (car line)))))
                                      (beg (cdr line))
                                      (end (+ (cdr line) (length (car line)) -1)))
                                  (if (s-blank? (s-trim (car line)))
                                      'newline ;sentinel representing line break
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

(defun origami-build-pair-tree (create open close positions)
  "Build the pair tree from CREATE.
Argument OPEN is the open symbol in type of string.  Argument CLOSE is
the close symbol in type of string.  POSITIONS is a list of cons cell
form by (syntax . point)."
  (cl-labels
      ((build (positions)
              ;; this is so horrible, but fast
              (let (acc beg (should-continue t))
                (while (and should-continue positions)
                  (cond ((equal (caar positions) open)
                         (if beg  ; go down a level
                             (let* ((res (build positions))
                                    (new-pos (car res)) (children (cdr res)))
                               (setq positions (cdr new-pos)
                                     acc (cons (funcall create beg (cdar new-pos) (length open) children)
                                               acc)
                                     beg nil))
                           ;; begin a new pair
                           (setq beg (cdar positions)
                                 positions (cdr positions))))
                        ((equal (caar positions) close)
                         (if beg  ; close with no children
                             (setq acc (cons (funcall create beg (cdar positions) (length open) nil)
                                             acc)
                                   positions (cdr positions)
                                   beg nil)
                           (setq should-continue nil)))))
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

(defun origami-build-pair-tree-2 (create positions)
  "Build pair list tree.

POSITIONS is from by a list of cons cell form by (syntax . point).
Notice POSITIONS' length must be an even number due to the list must be
pair up.  For instance,

  <0> (syntax . point),
  <1> (syntax . point),
  <2> (syntax . point),
  <3> (syntax . point), ...

Item N and the next item (N + 1) should be a pair; hence, N should always
be even number (if count starting from 0 and not 1)."
  (let ((index 0) (len (length positions))
        beg end offset pos-beg pos-end
        ov ovs)
    (when (origami-util-is-odd len)
      (if origami--strict-pair
          (error "Pair tree 2 should not have length of odd number: %s" len)
        (setq positions (origami--force-pair-positions positions))))
    (while (< index len)
      (setq pos-beg (nth index positions)
            pos-end (nth (1+ index) positions)
            beg (cdr pos-beg) end (cdr pos-end)
            offset (length (car pos-beg))
            ov (ignore-errors (funcall create beg end offset nil)))
      (when ov (push ov ovs))
      (cl-incf index 2))
    (reverse ovs)))

(defun origami-build-pair-tree-single (create syntax fn-filter)
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
    (let* ((positions (->> (origami-get-positions content syntax)
                           (-filter fn-filter)))
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
      (origami-build-pair-tree-2 create valid-positions))))

;;
;; (@* "Parsers" )
;;

(defvar origami-doc-faces
  '(font-lock-doc-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    hl-todo)
  "List of face that apply for docstring.")

(defun origami-doc-faces-p (obj)
  "Return non-nil if face at OBJ is within `origami-doc-faces' list."
  (origami-util-is-face obj origami-doc-faces))

(defun origami-filter-doc-face (position)
  "Filter for document face."
  (origami-doc-faces-p (car position)))

(defun origami-parser-triple-slash (create)
  "Parser for single line syntax triple slash."
  (origami-build-pair-tree-single create "///" 'origami-filter-doc-face))

(defun origami-parser-double-slash (create)
  "Parser for single line syntax double slash."
  (origami-build-pair-tree-single create "//" 'origami-filter-doc-face))

(defun origami-parser-single-sharp (create)
  "Parser for single line syntax single sharp."
  (origami-build-pair-tree-single create "#" 'origami-filter-doc-face))

(defun origami-parser-double-semi-colon (create)
  "Parser for single line syntax double semi-colon."
  (origami-build-pair-tree-single create ";;" 'origami-filter-doc-face))

(defun origami-parser-double-dash (create)
  "Parser for single line syntax double dash."
  (origami-build-pair-tree-single create "--" 'origami-filter-doc-face))

(defun origami-parser-double-colon (create)
  "Parser for single line syntax double colon."
  (origami-build-pair-tree-single create "::" 'origami-filter-doc-face))

(defun origami-parser-rem (create)
  "Parser for single line syntax REM."
  (origami-build-pair-tree-single create "[Rr][Ee][Mm]" 'origami-filter-doc-face))

;; TODO: tag these nodes? have ability to manipulate nodes that are tagged?
;; in a scoped fashion?
(defun origami-javadoc-parser (create)
  "Parser for Javadoc."
  (lambda (content)
    (let ((positions
           (->> (origami-get-positions content "/\\*\\|\\*/")
                (-filter 'origami-filter-doc-face))))
      (origami-build-pair-tree-2 create positions))))

(defun origami-python-doc-parser (create)
  "Parser for Python document string."
  (lambda (content)
    (let ((positions
           (->> (origami-get-positions content "\"\"\"")
                (-filter 'origami-filter-doc-face))))
      (origami-build-pair-tree-2 create positions))))

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
           (->> (origami-get-positions content "[{}]")
                (-filter (lambda (position)
                           (not (origami-util-comment-block-p (cdr position))))))))
      (origami-build-pair-tree create "{" "}" positions))))

(defun origami-c-macro-parser (create)
  "Parser for C style macro."
  (lambda (content)
    (let ((positions (origami-get-positions content "#if\\|#endif")))
      (origami-build-pair-tree create "#if" "#endif" positions))))

(defun origami-c-parser (create)
  "Parser for C."
  (let ((c-style (origami-c-style-parser create))
        (macros (origami-c-macro-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall javadoc content))
                                   (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall macros content)))))))

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
        (javadoc (origami-javadoc-parser create))
        (p-ts (origami-parser-triple-slash create))
        (p-ds (origami-parser-double-slash create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
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
      (python-mode)
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

(defun origami-parser-imenu-flat (create)
  "Origami parser producing folds for each imenu entry, without nesting."
  (lambda (content)
    (let ((orig-major-mode major-mode))
      (with-temp-buffer
        (insert content)
        (funcall orig-major-mode)
        (let* ((items
                (-as-> (imenu--make-index-alist t) items
                       (-flatten items)
                       (-filter 'listp items)))
               (positions
                (-as-> (-map #'cdr items) positions
                       (-filter 'identity positions)
                       (-map-when 'markerp 'marker-position positions)
                       (-filter 'natnump positions)
                       (cons (point-min) positions)
                       (-snoc positions (point-max))
                       (-sort '< positions)
                       (-uniq positions)))
               (ranges
                (-zip-pair positions (-map '1- (cdr positions))))
               (fold-nodes
                (--map
                 (-let*
                     (((range-beg . range-end) it)
                      (line-beg
                       (progn (goto-char range-beg)
                              (line-beginning-position)))
                      (offset
                       (- (min (line-end-position) range-end) line-beg))
                      (fold-node
                       (funcall create line-beg range-end offset nil)))
                   fold-node)
                 ranges)))
          fold-nodes)))))

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
            (setq acc (cons (funcall create beg end offset nil) acc)))
          (beginning-of-defun -1))
        (reverse acc)))))

(defun origami-elisp-parser (create)
  "Parser for Emacs Lisp."
  (origami-lisp-parser create "(def\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun origami-lua-core-parser (create)
  "Core parser for Lua."
  ;; TODO: do this
  )

(defun origami-lua-parser (create)
  "Parser for Lua."
  (let ((p-lua (origami-lua-core-parser create))
        (p-dd (origami-parser-double-dash create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall p-lua content))
                                   (origami-fold-root-node (funcall p-dd content)))))))

(defun origami-clj-parser (create)
  "Parser for Clojure."
  (origami-lisp-parser create "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(defun origami-scala-parser (create)
  "Parser for Scala."
  (let ((c-style (origami-c-style-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall javadoc content))
                                   (origami-fold-root-node (funcall c-style content)))))))

(defun origami-sh-parser (create)
  "Parser for Shell script."
  (origami-parser-single-sharp create))

(defun origami-markers-parser (start-marker end-marker)
  "Create a parser for simple start and end markers."
  (let ((regex (rx-to-string `(or ,start-marker ,end-marker))))
    (lambda (create)
      (lambda (content)
        (let ((positions (origami-get-positions content regex)))
          (origami-build-pair-tree create start-marker end-marker positions))))))

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
    (go-mode               . origami-c-style-parser)
    (java-mode             . origami-java-parser)
    (javascript-mode       . origami-js-parser)
    (js-mode               . origami-js-parser)
    (js2-mode              . origami-js-parser)
    (js3-mode              . origami-js-parser)
    (kotlin-mode           . origami-java-parser)
    (lisp-mode             . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (lua-mode              . origami-lua-parser)
    (objc-mode             . origami-objc-parser)
    (perl-mode             . origami-c-style-parser)
    (php-mode              . origami-java-parser)
    (python-mode           . origami-parser-imenu-flat)
    (rjsx-mode             . origami-js-parser)
    (rst-mode              . origami-parser-imenu-flat)
    (rust-mode             . origami-parser-imenu-flat)
    (scala-mode            . origami-scala-parser)
    (sh-mode               . origami-sh-parser)
    (triple-braces         . ,(origami-markers-parser "{{{" "}}}"))
    (typescript-mode       . origami-js-parser))
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
This happens only when summary length is larger than `origami-max-summary-length'."
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

(defun origami-extract-doc (doc-str sym)
  "Extract only document content from DOC-STR using SYM"
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
  (let* ((lines (origami-extract-doc doc-str sym)) (summary (nth 0 lines)))
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
  (setq doc-str (s-replace-regexp "<[/]*[^>]+." "" doc-str))
  (origami--generic-summary doc-str "///"))

(defun origami-javadoc-summary (doc-str)
  "Extract javadoc summary from DOC-STR."
  (origami--generic-summary doc-str "*"))

(defun origami-lua-doc-summary (doc-str)
  "Extract Lua document string from DOC-STR."
  ;; TODO: Implement this..
  (user-error "[INFO] There is no Lua document string parser yet"))

(defun origami-python-doc-summary (doc-str)
  "Extract Python document string from DOC-STR."
  (origami--generic-summary doc-str "\"\"\""))

(defun origami-c-macro-summary (doc-str)
  "Parse C macro summary from DOC-STR."
  (when (origami-util-is-face doc-str '(preproc-font-lock-preprocessor-background))
    (origami-doc-extract-summary doc-str "")))

(defun origami-c-summary (doc-str)
  "Summary parser for C from DOC-STR."
  (or (origami-javadoc-summary doc-str)
      (origami-c-macro-summary doc-str)))

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
    (java-mode         . origami-javadoc-summary)
    (javascript-mode   . origami-javadoc-summary)
    (js-mode           . origami-javadoc-summary)
    (js2-mode          . origami-javadoc-summary)
    (js3-mode          . origami-javadoc-summary)
    (kotlin-mode       . origami-javadoc-summary)
    (lua-mode          . origami-lua-doc-summary)
    (objc-mode         . origami-c-summary)
    (php-mode          . origami-javadoc-summary)
    (python-mode       . origami-python-doc-summary)
    (rjsx-mode         . origami-javadoc-summary)
    (scala-mode        . origami-javadoc-summary)
    (sh-mode           . origami-javadoc-summary)
    (typescript-mode   . origami-javadoc-summary))
  "Alist mapping major-mode to doc parser function."
  :type 'hook
  :group 'origami)

(provide 'origami-parsers)
;;; origami-parsers.el ends here
