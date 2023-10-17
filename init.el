;;; package --- This is init.el of the emacs configuation  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; email settings
(setq user-mail-address "yly1228@foxmail.com")


;;; startup options
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Welcome to Godalin's Emacs\n\n")


;;; set fonts(faces)
(set-face-attribute 'default nil
                    :family "Julia Mono"
                    :height 160
                    :weight 'normal
                    :width  'normal)
(set-fontset-font "fontset-default" 'han (font-spec :family "LXGW Wenkai"))


(keymap-global-set "C--" 'global-text-scale-adjust)
(keymap-global-set "C-=" 'global-text-scale-adjust)


(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)


;;; edit options: input base
(setq read-quoted-char-radix 16)
(setq tab-width 2)


;;; move through visual-lines / locigal-lines
(setq line-move-visual t)
(setq track-eol t)
(setq next-line-add-newlines nil)


;;; info of position
(global-display-line-numbers-mode t)
(size-indication-mode t)
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode t)
(electric-pair-mode t)

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)


;;; keymaps
(keymap-global-set "M-p" 'flymake-goto-prev-error)
(keymap-global-set "M-n" 'flymake-goto-next-error)

(keymap-global-set "C-z" 'eshell)
(keymap-global-set "C-<f5>" 'display-line-numbers-mode)
(keymap-global-set "<f5>" 'flyspell-mode)


;;; command: open config directory
(defun edit-init-config ()
  "Quickly open .emacs.d in the Dired buffer."
  (interactive)
  (dired "/home/godalin/.emacs.d"))


;;; package paths
(push "/home/godalin/.emacs.d/lisp" load-path)
(push "/home/godalin/.emacs.d/lisp/lang" load-path)


;;; file management
(recentf-mode t)
(keymap-global-set "C-x C-r" 'recentf-open)
(keymap-global-set "C-x r" 'recentf-open-files)

(global-auto-revert-mode t)
(auto-save-visited-mode t)


;;; no backup files
(setq make-backup-files nil)


;;; load other files
(require 'packages)
(require 'init-haskell)
(require 'init-racket)
(require 'init-tex)


;;; eglot mode
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format))


;;; agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))


;;; org-mode
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex racket-mode haskell-mode sly company-coq proof-general julia-mode counsel format-all company which-key))
 '(safe-local-variable-values '((eval turn-off-auto-fill))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)
;;; init.el ends here
