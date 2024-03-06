;;; init-package.el --- Initiation of the keymap of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; 3rd Packages

;; crux for edit enhancement
(use-package crux
  :bind
  ("C-c o" . 'crux-open-with)
  ("C-k" . 'crux-smart-kill-line))


;; hungry delete
(use-package hungry-delete
  :bind
  ("C-<backspace>" . 'kill-word-or-whitespace-backward))


;; edit: drag lines up and down
(use-package drag-stuff
  :bind
  ("M-<up>" . 'drag-stuff-up)
  ("M-<down>" . 'drag-stuff-down))


;; window movement
(use-package ace-window
  :bind
  ("M-o" . 'ace-window))


;; edit parentheses
(use-package paredit
  :bind
  (:map
   paredit-mode-map
   ("C-<left>" . nil)
   ("C-<right>" . nil)
   ;; slurping and barfing
   ("M-0" . 'paredit-forward-slurp-sexp)
   ("M-9" . 'paredit-backward-slurp-sexp)
   ("M-]" . 'paredit-forward-barf-sexp)
   ("M-[" . 'paredit-backward-barf-sexp))
  :hook
  (scheme-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (racket-mode . paredit-mode)
  (dune-mode . paredit-mode))


;;; Global Keybind Modify


;; unbind all global C-num/M-num/C-M-num keys
(dotimes (num 10)
  (let ((C-num (format "C-%d" num))
        (M-num (format "M-%d" num))
        (C-M-num (format "C-M-%d" num)))
    (keymap-global-unset C-num)
    (keymap-global-unset M-num)
    (keymap-global-unset C-M-num)))


;; fcitx and keyboard-quit
(keymap-global-set "C-g"
                   (defun keyboard-and-fcitx5-quit ()
                     (interactive)
                     (shell-command "fcitx5-remote -c")
                     (keyboard-quit)))


;; scroll operation
;; (keymap-global-set "C-v" (defun scroll-up-3-lines () (interactive) (scroll-up-line 3)))
;; (keymap-global-set "M-v" (defun scroll-down-3-lines () (interactive) (scroll-down-line 3)))


;; flymake
(keymap-global-set "M-p" 'flymake-goto-prev-error)
(keymap-global-set "M-n" 'flymake-goto-next-error)


;; flyspell
(keymap-global-set "<f5>" 'flyspell-mode)


;;; Custom System Map (which are dangerous)

(defvar-keymap custom-system-map
  :prefix 'Custom-System-prefix
  :doc "This map is for custom system functions such as reboot."
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


;;; Custom Function Map

(defvar-keymap custom-function-map
  :prefix 'Custom-Function-prefix
  :doc "This map is for customization."
  ;; org bindings
  "o a" 'org-agenda
  "o c" 'org-capture
  "o l" 'org-store-link
  "p" 'list-packages                    ;show all packages
  "r f" 'recentf-open                   ;recentf
  "r r" 'recentf-open-files
  ;; search
  "s" 'scratch-buffer                   ;scratch
  ;; tab line mode
  "t t" 'tab-line-mode                  ;toggle tab line
  ;; whitespace
  "w c" 'whitespace-mode
  "w w" 'delete-trailing-whitespace     ;whitespace
  ;; terminals
  "x" 'term                             ;term
  "z" 'eshell                           ;eshell
  )

(keymap-global-set "C-z" 'Custom-Function-prefix)
(keymap-global-set "C-z C-x" 'Custom-System-prefix)


;;; Custom Commands


;; quickly open language configuration
(defun open-init-language (lang)
  "Open a configuration file with the given language.
LANG: the programming language"
  (interactive "sSelect language: ")
  (let ((init-file (format "~/.emacs.d/lisp/lang/init-%s.el" lang)))
    (cond ((file-exists-p init-file) (find-file init-file))
          (t (message "language init file not found.")))))


;; best backward kill command
(defun kill-word-or-whitespace-backward (n &optional killflag)
  "Kill word if non-whitespace, or all whitespace if any.
N
KILLFLAG"
  (interactive "p\nP")
  (let ((last-char (preceding-char)))
    (if (member last-char (list ?\s ?\n ?\t ?\v))
        (hungry-delete-backward n killflag)
      (backward-kill-word n))))


(provide 'init-keymaps)

;;; init-keymaps.el ends here
