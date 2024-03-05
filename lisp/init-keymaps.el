;;; init-package.el --- Initiation of the keymap of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; crux for edit enhancement
(use-package crux
	:bind
	("C-c o" . 'crux-open-with)
	("C-k" . 'crux-smart-kill-line)
	)


;; hungry delete
(use-package hungry-delete
	:bind
	("C-<backspace>" .  'kill-word-or-whitespace-backward))


;; edit: drag lines up and down
(use-package drag-stuff
	:bind
	("M-<up>" . 'drag-stuff-up)
	("M-<down>" . 'drag-stuff-down))


;; window movement
(use-package ace-window
	:bind
	("M-o" . 'ace-window))


;;; Global Keymap Modifying

;; new window and focus
(keymap-global-set "C-x C-2" (defun split-window-focus-below ()
                               (interactive)
                               (split-window-below)
                               (other-window 1)))
(keymap-global-set "C-x C-3" (defun split-window-focus-right ()
                               (interactive)
                               (split-window-right)
                               (other-window 1)))

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


;;; Custom Keymaps


;; custom system map (which are dangerous)
(defvar-keymap custom-system-map
  :prefix 'Custom-System-prefix
  :doc "This map is for custom system functions such as reboot"

  ;; control emacs
  "C-r" 'restart-emacs
  "C-z" 'suspend-emacs

  ;; initiation files
  "C-c d" (defun open-init-dir      () (interactive) (dired "~/.emacs.d"))
  "C-c e" (defun open-init-evil     () (interactive) (find-file "~/.emacs.d/lisp/init-evil.el"))
  "C-c i" (defun open-init          () (interactive) (find-file "~/.emacs.d/init.el"))
  "C-c k" (defun open-init-keymaps  () (interactive) (find-file "~/.emacs.d/lisp/init-keymaps.el"))
  "C-c l" 'open-init-language
  "C-c o" (defun open-init-org      () (interactive) (find-file "~/.emacs.d/lisp/init-org.el"))
  "C-c p" (defun open-init-packages () (interactive) (find-file "~/.emacs.d/lisp/init-packages.el"))
  )


;; custom function map
(defvar-keymap custom-function-map
  :prefix 'Custom-Function-prefix
  :doc "This map is for customization"

	;; org bindings
  "o a" 'org-agenda
  "o c" 'org-capture
  "o l" 'org-store-link

	"p" 'list-packages										;show all packages

  "r f" 'recentf-open										;recentf
  "r r" 'recentf-open-files

  "s" 'scratch-buffer										;scratch

	"w c" 'whitespace-mode
  "w w" 'delete-trailing-whitespace			;whitespace

  "x" 'term															;term
	"z" 'eshell														;eshell
  )


(keymap-global-set "C-z" 'Custom-Function-prefix)
(keymap-global-set "C-z C-x" 'Custom-System-prefix)


(defvar-keymap function-toggle-map
	:prefix 'Function-Toggle-Prefix
	:doc "This is a keymap for toggling some functions"
	"q" 'paredit-mode
	"p" 'modus-themes-toggle
	)

;; (keymap-global-set "C-m" 'Function-Toggle-Prefix)

;; custom commands


;; configuration
(defun open-init-language (lang)
  "Open a configuration file with the given language.
LANG: The programming language"
  (interactive "sSelect language: ")
  (let ((init-file (format "~/.emacs.d/lisp/lang/init-%s.el" lang)))
    (cond ((file-exists-p init-file) (find-file init-file))
					(t (message "language init file not found.")))))


;; best backward kill command
(defun kill-word-or-whitespace-backward (n &optional killflag)
	(interactive "p\nP")
	(let ((last-char (preceding-char)))
		(if (member last-char (list ?\s ?\n ?\t ?\v))
				(hungry-delete-backward n killflag)
			(backward-kill-word n))))


(provide 'init-keymaps)
;;; init-keymaps.el ends here
