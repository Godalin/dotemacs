;;; init-keymaps.el --- Fancy Keymaps 4 Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; crux
(use-package crux
  :defer t)

;;; hungry delete
(use-package hungry-delete
  :defer t
  :bind
  ("C-<backspace>" . kill-word-or-whitespace-backward))

;;; edit: drag lines up and down
(use-package drag-stuff
  :defer t
  :bind ((:map prog-mode-map
               ("M-<up>"   . drag-stuff-up)
               ("M-<down>" . drag-stuff-down))
         (:map text-mode-map
               ("M-<up>"   . drag-stuff-up)
               ("M-<down>" . drag-stuff-down))))

;;; edit parentheses
(use-package paredit
  :defer t
  :diminish paredit-mode
  :bind (:map paredit-mode-map
              ("C-<left>" . nil)
              ("C-<right>" . nil)
              ;; slurping and barfing
              ("M-0" . paredit-forward-slurp-sexp)
              ("M-9" . paredit-backward-slurp-sexp)
              ("M-]" . paredit-forward-barf-sexp)
              ("M-[" . paredit-backward-barf-sexp))
  :hook
  (scheme-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (racket-mode . paredit-mode)
  (dune-mode . paredit-mode))



;;; Global Keybind Modify

;;; Unset annoying keys
;;; The major difference from vanilla Emacs is that we do not
;;; use the following two keys to control when to exit Emacs.
(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-x C-z")

;;; And do not passing prefix arguments with `C-<num>' or `M-<num>'
;;; unbind all global C-num/M-num/C-M-num keys
(dotimes (num 10)
  (let ((C-num (format "C-%d" num))
        (M-num (format "M-%d" num))
        (C-M-num (format "C-M-%d" num)))
    (keymap-global-unset C-num)
    (keymap-global-unset M-num)
    (keymap-global-unset C-M-num)))

(keymap-global-set
 "C-x C-2" (lambda () (interactive)
             (split-window-below)
             (other-window 1)))

(keymap-global-set
 "C-x C-3" (lambda () (interactive)
             (split-window-right)
             (other-window 1)))

;;; do not use the wheel with modifier keys
;;; (which scales the fonts by default)
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")
(keymap-global-unset "S-<wheel-up>")
(keymap-global-unset "S-<wheel-down>")
(keymap-global-unset "M-<wheel-up>")
(keymap-global-unset "M-<wheel-down>")
(keymap-global-unset "C-M-<wheel-up>")
(keymap-global-unset "C-M-<wheel-down>")

;;; for Linux
(when (eq system-type 'gnu/linux)

  ;; quit fcitx with advice systems
  (defun keyboard-quit-then ()
    (shell-command "fcitx5-remote -c"))

  (advice-add 'keyboard-quit :before
              #'keyboard-quit-then))

;;; TODO restart emacs with server
(defun restart-emacs-w/server ()
  (interactive))

;;; TODO kill emacs with server
(defun kill-emacs-w/server ()
  (interactive))

;;; Custom System Map (Dangerous)
(defvar-keymap custom-system-map
  :prefix 'Custom-System-prefix
  :doc "This map is for custom system functions such as reboot."

  ;; control emacs
  "c" (lambda ()
        (interactive)
        (if (frame-parameter nil 'client)
            (delete-frame)
          (save-buffers-kill-emacs)))
  "C-r" 'restart-emacs
  "C-z" 'suspend-emacs

  ;; initialization files
  "C-c d" (lambda () (interactive) (dired "~/.config/emacs"))
  "C-c e" (lambda () (interactive) (find-file "~/.config/emacs/lisp/init-evil.el"))
  "C-c i" (lambda () (interactive) (find-file "~/.config/emacs/init.el"))
  "C-c k" (lambda () (interactive) (find-file "~/.config/emacs/lisp/init-keymaps.el"))
  "C-c l" 'open-init-language
  "C-c o" (lambda () (interactive) (find-file "~/.config/emacs/lisp/init-org.el"))
  "C-c p" (lambda () (interactive) (find-file "~/.config/emacs/lisp/init-packages.el"))
  )

;;; Custom Function Map
(defvar-keymap custom-function-map
  :prefix 'Custom-Function-prefix
  :doc "This map is for customization."

  ;; org bindings
  "o a" 'org-agenda
  "o c" 'org-capture
  "o l" 'org-store-link
  "p"   'list-packages                  ; show all packages
  "r f" 'recentf-open                   ; recentf
  "r r" 'recentf-open-files

  ;; tab line mode
  "t t" 'tab-line-mode                  ; toggle tab line

  ;; whitespace
  "w c" 'whitespace-mode                ; whitespace mode
  "w t" (lambda ()                           ; untabify the whole buffer
          (interactive)
          (mark-whole-buffer)
          (untabify))
  "w w" 'delete-trailing-whitespace     ; whitespace

  "x"   'sr-speedbar-toggle             ; sr-speedbar
  "z"   'eshell-toggle                  ; eshell

  ;; crux
  "SPC o" 'crux-open-with
  "SPC e" 'crux-eval-and-replace
  "SPC d" 'crux-duplicate-current-line-or-region
  "SPC c d" 'crux-duplicate-and-comment-current-line-or-region
  )

;;; replace global keymaps
(keymap-global-set "C-z"     #'Custom-Function-prefix)
(keymap-global-set "C-z C-x" #'Custom-System-prefix)



;;; Start a mode map over global map

(defvar-keymap fancy-keys-mode-map
  :doc "My custom keymap mode base map")

(key-chord-define fancy-keys-mode-map
                  "  " #'Custom-System-prefix)
(key-chord-define fancy-keys-mode-map
                  "xx" #'execute-extended-command)

(define-minor-mode fancy-keys-mode
  "My custom keymap mode for this configuration."
  :global t
  :lighter " ⟨FcyK⟩"
  :keymap fancy-keys-mode-map)



;;; Custom Commands

;;; quickly open language configuration
(defun open-init-language (lang)
  "Open a configuration file with the given language.
LANG: the programming language"
  (interactive "sSelect language: ")
  (let ((init-file (format "~/.config/emacs/lisp/lang/init-%s.el" lang)))
    (cond ((file-exists-p init-file) (find-file init-file))
          (t (message "language init file not found.")))))

;;; best backward kill command
(defun kill-word-or-whitespace-backward (n &optional killflag)
  "Kill word if non-whitespace, or all whitespace if any.
N: the prefix argument
KILLFLAG: i do not know what this is"
  (interactive "p\nP")
  (let ((last-char (preceding-char)))
    (if (member last-char (list ?\s ?\n ?\t ?\v))
        (hungry-delete-backward n killflag)
      (backward-kill-word n))))



(provide 'init-keymaps)

;;; init-keymaps.el ends here
