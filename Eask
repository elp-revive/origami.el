;; -*- mode: eask; lexical-binding: t -*-

(package "origami"
         "4.1"
         "Flexible text folding")

(website-url "https://github.com/elp-revive/origami.el")
(keywords "folding")

(package-file "origami.el")
(files "*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "dash")
(depends-on "fringe-helper")
(depends-on "s")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
