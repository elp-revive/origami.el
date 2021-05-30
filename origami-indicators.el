;;; origami-indicators.el --- Display indicators for origami  -*- lexical-binding: t -*-

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
;; Display indicators for origami
;;

;;; Code:

(require 'fringe-helper)

(require 'origami-util)

(declare-function origami-toggle-node "origami.el")
(declare-function origami-open-node "origami.el")
(declare-function origami-tree-overlays "origami.el")

(defcustom origami-indicators nil
  "Display indicators on the left/right fringe, if nil don't render."
  :type '(choice (const :tag "none" nil)
                 (const :tag "On the right fringe" right-fringe)
                 (const :tag "On the left fringe" left-fringe))
  :group 'origami)

(defcustom origami-indicators-priority 30
  "Indicators fringe priority."
  :type 'integer
  :group 'origami)

(defcustom origami-indicators-face-function nil
  "Function call when apply to indicators face."
  :type 'function
  :group 'origami)

(fringe-helper-define 'origami-indicators-fr-plus nil
  "XXXXXXX"
  "X.....X"
  "X..X..X"
  "X.XXX.X"
  "X..X..X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'origami-indicators-fr-minus nil
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'origami-indicators-fr-minus-tail nil
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'origami-indicators-fr-center nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX...")

(fringe-helper-define 'origami-indicators-fr-end-left nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XXXXX" "...XXXXX"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(fringe-helper-define 'origami-indicators-fr-end-right nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "XXXXX..." "XXXXX..."
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(defun origami-indicators-click-fringe (event)
  "EVENT click on fringe."
  (interactive "e")
  (let ((current-fringe (nth 1 (car (cdr event)))) ovs ov cur-ln)
    (when (eq current-fringe origami-indicators)
      (mouse-set-point event)
      (beginning-of-line)
      (setq cur-ln (line-number-at-pos (point)))
      (setq ovs (append (origami-util-overlays-in 'type 'origami-indicators-fr-plus)
                        (origami-util-overlays-in 'type 'origami-indicators-fr-minus)
                        (origami-util-overlays-in 'type 'origami-indicators-fr-minus-tail)))
      (when ovs
        (setq ov (cl-some
                  (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                  ovs))
        (when ov
          (end-of-line)
          (call-interactively #'origami-toggle-node))))))

(defun origami-indicators--create-overlay-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'creator 'origami-indicators)
    ov))

(defun origami-indicators--create-overlays (beg end)
  "Return a list of indicator overlays from BEG to END."
  (let ((ov-lst '()))
    (save-excursion
      (goto-char beg)
      (while (and (<= (line-beginning-position) end) (not (eobp)))
        (push (origami-indicators--create-overlay-at-point) ov-lst)
        (forward-line 1)))
    (origami-indicators--update-overlays (reverse ov-lst) t)))

(defun origami-indicators--get-priority (bitmap)
  "Get priority by BITMAP."
  (let ((prior origami-indicators-priority))
    (cl-case bitmap
      (origami-indicators-fr-plus (+ prior 2))
      (origami-indicators-fr-minus (+ prior 2))
      (origami-indicators-fr-minus-tail (+ prior 2))
      (origami-indicators-fr-end-left (+ prior 1))
      (origami-indicators-fr-end-right (+ prior 1))
      (t prior))))

(defun origami-indicators--get-string (show ov bitmap)
  "Return the string properties for OV by SHOW and BITMAP."
  (let* ((face (or (and (functionp origami-indicators-face-function)
                        (funcall origami-indicators-face-function (overlay-start ov)))
                   'origami-fold-fringe-face))
         (str (propertize "…" 'display `(,origami-indicators ,bitmap ,face))))
    (if show str
      (cl-case bitmap
        (origami-indicators-fr-plus str)
        (origami-indicators-fr-minus nil)
        (origami-indicators-fr-minus-tail nil)
        (origami-indicators-fr-end-left nil)
        (origami-indicators-fr-end-right nil)
        (t nil)))))

(defun origami-indicators--active-ov (show ov bitmap)
  "SHOW the indicator OV with BITMAP."
  (when (and origami-indicators (overlayp ov))
    (overlay-put ov 'origami-indicators-active show)
    (overlay-put ov 'type bitmap)
    (overlay-put ov 'priority (origami-indicators--get-priority bitmap))
    (overlay-put ov 'before-string (origami-indicators--get-string show ov bitmap))))

(defun origami-indicators--get-end-fringe ()
  "Return end fringe bitmap according to variable `origami-indicators'."
  (when origami-indicators  ; accept nil value
    (cl-case origami-indicators
      (left-fringe 'origami-indicators-fr-end-left)
      (right-fringe 'origami-indicators-fr-end-right)
      (t (user-error "Invalid indicators fringe type: %s" origami-indicators)))))

(defun origami-indicators--update-overlays (ov-lst show)
  "SHOW indicators overlays OV-LST."
  (let* ((len (length ov-lst))
         (len-1 (1- len))
         (first-ov (nth 0 ov-lst))
         (last-ov (nth len-1 ov-lst))
         (index 1))
    (origami-indicators--active-ov
     show first-ov
     (if show
         (if (> len 1)
             'origami-indicators-fr-minus-tail 'origami-indicators-fr-minus)
       'origami-indicators-fr-plus))
    (when (> len 1)
      (origami-indicators--active-ov show last-ov (origami-indicators--get-end-fringe)))
    (while (< index len-1)
      (origami-indicators--active-ov show (nth index ov-lst) 'origami-indicators-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Timer" )
;;

(defcustom origami-indicators-time 0.5
  "Indicators refresh rate in time."
  :type 'float
  :group 'origami)

(defvar-local origami-indicators--timer nil
  "Timer for update indicators.")

(defun origami-indicators--refresh (&optional buffer &rest _)
  "Refresh indicator overlays."
  (origami-util-with-current-buffer buffer
    (ignore-errors (call-interactively #'origami-open-node))  ; first rebuild tree
    ;; Remove other invalid obsolete overlays
    (let ((ovs (origami-tree-overlays buffer)))
      (dolist (ov (origami-util-overlays-in 'creator 'origami))
        (unless (memq ov ovs) (delete-overlay ov))))
    ;; Remove all indicator overlays
    (remove-overlays (point-min) (point-max) 'creator 'origami-indicators)
    ;; Reapply indicator overlays
    (let ((ovs (overlays-in (point-min) (point-max))) start end tmp-ovs)
      (dolist (ov ovs)
        (when (eq 'origami (overlay-get ov 'creator))
          (setq start (overlay-start ov) end (overlay-end ov)
                tmp-ovs (overlay-get ov 'ind-ovs))
          (unless (equal start end)
            (when (listp tmp-ovs) (mapc #'delete-overlay tmp-ovs))
            (overlay-put ov 'ind-ovs (origami-indicators--create-overlays start end))))))))

(defun origami-indicators--start-timer (&rest _)
  "Start refresh timer."
  (when origami-indicators
    (when (timerp origami-indicators--timer) (cancel-timer origami-indicators--timer))
    (setq origami-indicators--timer
          (run-with-idle-timer origami-indicators-time nil
                               #'origami-indicators--refresh (current-buffer)))))

(provide 'origami-indicators)
;;; origami-indicators.el ends here
