;; email address
(setq user-mail-address "yly1228@foxmail.com")

;; startup options
(setq inhibit-startup-screen t)

;; ui settings
(setq resize-mini-windows t)

;; (setq initial-buffer-choice "~/.emacs.d/init.el")
(setq initial-scratch-message ";; Welcome to Godalin's Emacs\n\n")
(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)

;; edit options
(setq read-quoted-char-radix 16)

;; move through visual-lines / locigal-lines
(setq line-move-visual t)
(setq track-eol t)
(setq next-line-add-newlines nil)

;; info of position
(global-display-line-numbers-mode t)
(size-indication-mode t)
;; (global-column-number-mode t)
;; (global-line-number-mode t)
(global-hl-line-mode t)

;; keymaps
(keymap-global-set "C-z" 'eshell)
(keymap-global-set "C-<f5>" 'display-line-numbers-mode)
(keymap-global-set "<f5>" 'flyspell-mode)

;; ede: ide-like
(global-ede-mode t)

;; icomplete mode
(icomplete-mode t)

(load "lisp/packages.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-stylish-on-save t)
 '(package-selected-packages '(auctex haskell-mode which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "ADBO" :family "GoMono Nerd Font")))))
