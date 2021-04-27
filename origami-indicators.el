;;; origami-indicators.el --- Flexible text folding  -*- lexical-binding: t -*-

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Keywords: utility tool
;; URL: https://github.com/emacs-origami/origami.el

;; The MIT License (MIT)

;; Copyright (c) 2021 Jen-Chieh Shen

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
;; Display indicators for origami.
;;

;;; Code:

(require 'fringe-helper)

(require 'origami-util)

(defcustom origami-indicators nil
  "Display indicators on the left/right fringe, if nil don't render.k"
  :type '(choice (const :tag "none" nil)
                 (const :tag "On the right fringe" right-fringe)
                 (const :tag "On the left fringe" left-fringe))
  :group 'origami)

(defcustom origami-indicators-priority 30
  "Indicators fringe priority."
  :type 'integer
  :group 'origami)

(fringe-helper-define 'origami-fr-plus nil
  "XXXXXXX"
  "X.....X"
  "X..X..X"
  "X.XXX.X"
  "X..X..X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'origami-fr-minus nil
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'origami-fr-minus-tail nil
  "......." "......." "......." "......." "......."
  "......." "......." "......." "......." "......."
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X..." "...X..." "...X...")

(fringe-helper-define 'origami-fr-center nil
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X...")

(fringe-helper-define 'origami-fr-end nil
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...X..." "...X..." "...X..." "...X..."
  "...X..." "...XXXX" "......."
  "......." "......." "......." "......." "......."
  "......." "......." "......." "......." ".......")

(defun origami-click-fringe (event)
  "EVENT click on fringe."
  (interactive "e")
  (let ((current-fringe (nth 1 (car (cdr event)))))
    (when (eq current-fringe origami-indicators)
      (mouse-set-point event)
      (end-of-line)
      (call-interactively #'origami-toggle-node))))

(defun origami-ind--create-overlay-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'creator 'origami)
    ov))

(defun origami-ind--create-overlays (beg end)
  "Return a list of indicator overlays from BEG to END."
  (let ((ov-lst '()))
    (save-excursion
      (goto-char beg)
      (while (<= (line-beginning-position) end)
        (push (origami-ind--create-overlay-at-point) ov-lst)
        (forward-line 1)))
    (origami-ind--update-overlays (reverse ov-lst) t)))

(defun origami-ind--get-priority (bitmap)
  "Get priority by BITMAP."
  (let ((prior origami-indicators-priority))
    (cl-case bitmap
      (origami-fr-plus (+ prior 2))
      (origami-fr-minus (+ prior 2))
      (origami-fr-minus-tail (+ prior 2))
      (origami-fr-end (+ prior 1))
      (t prior))))

(defun origami-ind--get-string (show bitmap)
  "Return the string properties by SHOW and BITMAP."
  (let ((str (propertize "…" 'display `(,origami-indicators ,bitmap origami-fold-fringe-face))))
    (if show str
      (cl-case bitmap
        (origami-fr-plus str)
        (origami-fr-minus nil)
        (origami-fr-minus-tail nil)
        (origami-fr-end nil)
        (t nil)))))

(defun origami--active-ind-ov (show ov bitmap)
  "Set active the indicator OV with BITMAP."
  (when origami-indicators
    (overlay-put ov 'origami-indicators-active show)
    (overlay-put ov 'priority (origami-ind--get-priority bitmap))
    (overlay-put ov 'before-string (origami-ind--get-string show bitmap))))

(defun origami-ind--update-overlays (ov-lst show)
  "Update indicators overlays OV-LST."
  (let* ((len (length ov-lst))
         (len-1 (1- len))
         (first-ov (nth 0 ov-lst))
         (last-ov (nth len-1 ov-lst))
         (index 1))
    (origami--active-ind-ov show first-ov
                            (if show
                                (if (> len 1)
                                    'origami-fr-minus-tail 'origami-fr-minus)
                              'origami-fr-plus))
    (when (> len 1)
      (origami--active-ind-ov show last-ov 'origami-fr-end))
    (while (< index len-1)
      (origami--active-ind-ov show (nth index ov-lst) 'origami-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Timer" )
;;

(defvar-local origaim-ind--timer nil
  "Timer for update indicators.k")

(defcustom origami-indicators-time 0.8
  "Indicators refresh rate in time."
  :type 'float
  :group 'origami)

(defun origami-ind--after-change-functions (_beg _end _len)
  "After change functions."
  (when (timerp origaim-ind--timer) (cancel-timer origaim-ind--timer))
  (setq origaim-ind--timer (run-with-idle-timer origami-indicators-time nil
                                                #'origami-reset (current-buffer))))

(provide 'origami-indicators)
;;; origami-indicators.el ends here
