;;; init-package.el --- Initiation of the keymap of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;; fcitx and keyboard-quit
(keymap-global-set "C-g" (defun keyboard-and-fcitx5-quit ()
													 (interactive)
													 (shell-command "fcitx5-remote -c")
													 (keyboard-quit)))

;; scroll operation
(keymap-global-set "C-v" (defun scroll-up-3-lines () (interactive) (scroll-up-line 3)))
(keymap-global-set "M-v" (defun scroll-down-3-lines () (interactive) (scroll-down-line 3)))

;; flymake
(keymap-global-set "M-p" 'flymake-goto-prev-error)
(keymap-global-set "M-n" 'flymake-goto-next-error)

;; flyspell
(keymap-global-set "<f5>" 'flyspell-mode)


;; custom system map (which are dangerous)
(defvar-keymap custom-system-map
  :prefix 'Custom-System-prefix
  :doc "This map is for custom system functions such as reboot"

  ;; control emacs
  "C-r" 'restart-emacs
  "C-z" 'suspend-emacs

  ;; initiation files
  "C-c d" (defun open-init-dir () (interactive) (dired "~/.emacs.d"))
  "C-c e" (defun open-init-evil () (interactive) (find-file "~/.emacs.d/lisp/init-evil.el"))
  "C-c i" (defun open-init () (interactive) (find-file "~/.emacs.d/init.el"))
  "C-c k" (defun open-init-keymaps () (interactive) (find-file "~/.emacs.d/lisp/init-keymaps.el"))
  "C-c l" 'open-init-language
  "C-c o" (defun open-init-org () (interactive) (find-file "~/.emacs.d/lisp/init-org.el"))
  "C-c p" (defun open-init-packages () (interactive) (find-file "~/.emacs.d/lisp/init-packages.el"))
  )


;; custom function map
(defvar-keymap custom-function-map
  :prefix 'Custom-Function-prefix
  :doc "This map is for customization"

  "o a" 'org-agenda
  "o c" 'org-capture
  "o l" 'org-store-link

	"p" 'list-packages										;show all packages

  "r f" 'recentf-open										;recentf
  "r r" 'recentf-open-files

  "s" 'scratch-buffer										;scratch

	"w c" 'delete-trailing-whitespace
  "w w" 'whitespace-mode								;whitespace

  "x" 'vterm														;vterm
	"z" 'eshell														;eshell
  )


;; bind custom-map to "C-z" and "C-z C-x"
(keymap-global-set "C-z" 'Custom-Function-prefix)
(keymap-global-set "C-z C-x" 'Custom-System-prefix)


;; custom commands


;; configuration
(defun open-init-language (lang)
  "Open a configuration file with the given language.
LANG: The programming language"
  (interactive "sSelect language: ")
  (let ((init-file (format "~/.emacs.d/lisp/lang/init-%s.el" lang)))
    (cond ((file-exists-p init-file) (find-file init-file))
	  (t (message "language init file not found.")))))


(provide 'init-keymaps)
;;; init-keymaps.el ends here
