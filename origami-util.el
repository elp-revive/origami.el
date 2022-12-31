;;; origami-util.el --- Utility module  -*- lexical-binding: t -*-

;; The MIT License (MIT)

;; Copyright (c) 2020-2023 Jen-Chieh Shen

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
;; Utility module.
;;

;;; Code:

(require 'cl-lib)

;;
;; (@* "Log" )
;;

(defvar origami-show-log nil
  "If non-nil, show debug message.")

(defun origami-log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when origami-show-log (apply 'message fmt args)))

;;
;; (@* "Macros" )
;;

(defmacro origami-util-with-current-buffer (buffer-or-name &rest body)
  "Safe to use function `with-current-buffer'."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer-or-name)
     (with-current-buffer ,buffer-or-name (progn ,@body))))

;;
;; (@* "Point" )
;;

(defun origami-util-pos-line-beg (pos)
  "Return the line beginning position after moved to POS."
  (save-excursion (goto-char pos) (line-beginning-position)))

(defun origami-util-pos-line-end (pos)
  "Return the line end position after moved to POS."
  (save-excursion (goto-char pos) (line-end-position)))

(defun origami-util-comment-block-p (&optional pos)
  "Return non-nil if POS is inside a comment block."
  (unless pos (setq pos (point)))
  (save-excursion (goto-char pos) (nth 4 (syntax-ppss))))

(defun origami-util-string-block-p (&optional pos)
  "Return non-nil if POS is inside a string."
  (unless pos (setq pos (point)))
  (save-excursion (goto-char pos) (nth 8 (syntax-ppss))))

(defun origami-util-comment-or-string-p (&optional pos)
  "Return non-nil if POS is inside a comment or string."
  (unless pos (setq pos (point)))
  (or (origami-util-comment-block-p pos) (origami-util-string-block-p pos)))

;;
;; (@* "Overlay" )
;;

(defun origami-util-ov-string (ov)
  "Return string from OV."
  (substring (buffer-string) (1- (overlay-start ov)) (1- (overlay-end ov))))

(defun origami-util-overlays-at (prop name &optional pos)
  "Return overlays with PROP of NAME at POS."
  (unless pos (setq pos (point)))
  (let ((lst '()) (ovs (overlays-at pos)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

(defun origami-util-overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (unless beg (setq beg (point-min))) (unless end (setq end (point-max)))
  (let ((lst '()) (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

;;
;; (@* "Face" )
;;

(defun origami-util-get-face (obj trim)
  "Return face name from OBJ.

If argument TRIM is non-nil, trim the OBJ."
  (get-text-property 0 'face (if trim (string-trim obj) obj)))

(defun origami-util-is-face (obj lst-face &optional trim)
  "Return non-nil if OBJ's face is define inside list LST-FACE.

Optional argument TRIM, see function `origami-util-get-face'."
  (unless (listp lst-face) (setq lst-face (list lst-face)))
  (let ((faces (origami-util-get-face obj trim)))
    (cond ((listp faces)
           (cl-some (lambda (face) (memq face lst-face)) faces))
          (t (memq faces lst-face)))))

;;
;; (@* "String" )
;;

(defun origami-util-string-compare-p (regexp str type &optional ignore-case)
  "Compare STR with REGEXP by TYPE.

Argument TYPE can be on of the following symbol.

  * regex - uses function `string-match-p'.  (default)
  * strict - uses function `string='.
  * prefix - uses function `string-prefix-p'.
  * suffix - uses function `string-suffix-p'.

Optional argument IGNORE-CASE is only uses when TYPE is either symbol `prefix'
or `suffix'."
  (cl-case type
    (strict (string= regexp str))
    (prefix (string-prefix-p regexp str ignore-case))
    (suffix (string-suffix-p regexp str ignore-case))
    (t (string-match-p regexp str))))

(defun origami-util-seq-omit-string (seq &optional trim)
  "Return a list of omitted empty string and nil from SEQ.
If optional argument TRIM is non-nil; then trim all string in SEQ."
  (let (lst)
    (dolist (item seq)
      (when trim (setq item (string-trim item)))
      (unless (string-empty-p item)
        (push item lst)))
    (reverse lst)))

(defun origami-util-contain-list-string-regexp (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'."
  (cl-some (lambda (elm) (string-match-p elm in-str)) in-list))

(defun origami-util-contain-list-string (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'.
This function wrapped IN-STR with function `regexp-quote'."
  (cl-some (lambda (elm) (string-match-p (regexp-quote elm) in-str)) in-list))

(defun origami-util-contain-list-type-str (in-list in-str type)
  "Return non-nil if IN-STR is listed in IN-LIST.

Argument TYPE see function `origami-util-string-compare-p' for more information."
  (cl-some (lambda (elm) (origami-util-string-compare-p elm in-str type)) in-list))

;;
;; (@* "Regular Expression" )
;;

(defun origami-util-keywords-regex (keywords)
  "Turn a list of KEYWORDS to a keyword regular expression."
  (let ((key-str "") (len (length keywords)) keyword (index 0))
    (while (< index len)
      (setq keyword (nth index keywords)
            key-str (concat key-str keyword (if (= index (1- len)) "" "\\|")))
      (cl-incf index))
    (format "\\(s*%s\\)\\_>" key-str)))

(defun origami-util-comment-regex (symbols)
  "Turn a list of KEYWORDS to a keyword regular expression."
  (let ((key-str "") (len (length symbols)) keyword (index 0))
    (while (< index len)
      (setq keyword (nth index symbols)
            key-str (concat key-str keyword (if (= index (1- len)) "" "\\|")))
      (cl-incf index))
    (format "\\(s*%s\\)" key-str)))

;;
;; (@* "Math" )
;;

(defun origami-util-is-odd (in-val)
  "Check IN-VAL an odd number."
  (= (% in-val 2) 1))

(defun origami-util-is-even (in-val)
  "Check IN-VAL an even number."
  (not (origami-util-is-odd in-val)))

;;
;; (@* "Functions" )
;;

(defun origami-util-function-offset (fnc beg match)
  "Call FNC starting from BEG.
Argument MATCH is for callback."
  (let ((result (if (not fnc) (length match)
                  (save-excursion (goto-char beg)
                                  (- (funcall fnc match) beg)))))
    (unless result
      (user-error "Something went wrong while offsetting region: `%s` at `%s`" match beg))
    result))

(provide 'origami-util)
;;; origami-util.el ends here
