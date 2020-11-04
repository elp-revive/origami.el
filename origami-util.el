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

(defun origami-util-ov-string (ov)
  "Return string from OV."
  (substring (buffer-string) (overlay-start ov) (overlay-end ov)))

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

(provide 'origami-util)
;;; origami-util.el ends here
