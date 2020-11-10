;;; origami-util.el --- Flexible text folding  -*- lexical-binding: t -*-

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Version: 2.1
;; Keywords: utility tool
;; URL: https://github.com/jcs-elpa/origami.el

;; The MIT License (MIT)

;; Copyright (c) 2020 Jen-Chieh Shen

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

;;
;; (@* "Point" )
;;

(defun origami-util-pos-line-beg (pos)
  "Return the line beginning position after moved to POS."
  (save-excursion (goto-char pos) (line-beginning-position)))

(defun origami-util-pos-line-end (pos)
  "Return the line end position after moved to POS."
  (save-excursion (goto-char pos) (line-end-position)))

(defun origami-util-comment-block-p (pos)
  "Return non-nil if POS is inside a comment block."
  (save-excursion (goto-char pos) (nth 4 (syntax-ppss))))

(defun origami-util-string-block-p (pos)
  "Return non-nil if POS is inside a string."
  (save-excursion (goto-char pos) (nth 8 (syntax-ppss))))

(defun origami-util-comment-or-string-p (pos)
  "Return non-nil if POS is inside a comment or string."
  (or (origami-util-comment-block-p pos) (origami-util-string-block-p pos)))

;;
;; (@* "Overlay" )
;;

(defun origami-util-ov-string (ov)
  "Return string from OV."
  (substring (buffer-string) (1- (overlay-start ov)) (1- (overlay-end ov))))

;;
;; (@* "Face" )
;;

(defun origami-util-get-face (obj)
  "Return face name from OBJ."
  (get-text-property 0 'face obj))

(defun origami-util-is-face (obj lst-face)
  "Return non-nil if OBJ's face is define inside list LST-FACE."
  (unless (listp lst-face) (setq lst-face (list lst-face)))
  (let ((faces (origami-util-get-face obj)))
    (cond ((listp faces)
           (cl-some (lambda (face) (memq face lst-face)) faces))
          (t (memq faces lst-face)))))

;;
;; (@* "String" )
;;

(defun origami-seq-omit-string (seq &optional trim)
  "Return a list of omitted empty string and nil from SEQ.
If optional argument TRIM is non-nil; then trim all string in SEQ."
  (let (lst)
    (dolist (item seq)
      (when trim (setq item (string-trim item)))
      (unless (string-empty-p item)
        (push item lst)))
    (reverse lst)))

(defun origami-is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

;;
;; (@* "Math" )
;;

(defun origami-util-is-odd (in-val)
  "Check IN-VAL an odd number."
  (= (% in-val 2) 1))

(defun origami-util-is-even (in-val)
  "Check IN-VAL an even number."
  (not (origami-util-is-odd in-val)))

(provide 'origami-util)
;;; origami-util.el ends here
