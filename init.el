;;; package --- init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; use the use-package package
(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure t)
  (use-package-always-defer nil)
	(use-package-enable-imenu-support t))

;;; use `use-package' to deal with `package'
(use-package package
  :ensure nil
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

;;; `dimish' for the minor modes in mod line
(use-package diminish)

;;; load environment variables
(use-package exec-path-from-shell
  :custom (exec-path-from-shell-arguments nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;;; custom file
(use-package emacs
  :ensure nil
  :custom
  (custom-file
   (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load custom-file 'no-error 'no-message))

(use-package emacs
  :ensure nil
  :custom
  (user-full-name "Linyu Yang")
  (user-mail-address "yly1228@foxmail.com")
  (use-short-answers t)
  (inhibit-startup-screen nil)
  (initial-scratch-message
   ";;; Welcome to Godalin's Emacs\n\n")
  (scroll-preserve-screen-position
   t "remember point positions"))

(use-package files
  :custom
  (confirm-kill-processes nil "auto kill processes when exit")
  (make-backup-files nil "do not create backup files")
  :hook (after-init . auto-save-visited-mode))

;;; Xia Wu WenKai
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default" 'han "LXGW Wenkai"))

;;; visual line mode
(use-package simple
  :ensure nil
  :custom
  (line-move-visual t)
  (track-eol t)
  (visual-line-fringe-indicators t)
  (word-wrap-by-category t)
  (read-quoted-char-radix 16 "input method, use hex code")
  :hook ((after-init . (size-indication-mode
                        line-number-mode
                        column-number-mode))
         (prog-mode . global-visual-line-mode)
         (text-mode . global-visual-line-mode)))

;;; space/tab/indention related
(setopt tab-width 2)                   ; 2 spaces = 1 tab
(setopt tab-always-indent t)           ; only indent at left
(setopt indent-tabs-mode nil)          ; do not use tabs for indention
(setq indent-line-function #'tab-to-tab-stop) ; use a trivial indention function

;;; display
(add-hook 'after-init-hook #'tab-bar-mode)

(use-package pixel-scroll
  :ensure nil
  :hook
  (after-init . pixel-scroll-precision-mode))

;;; prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((emacs-lisp-mode . prettify-symbols-mode)
         (prog-mode
          . (lambda () (setopt show-trailing-whitespace t
                          indicate-empty-lines t)))))
;;; text-mode
(use-package text-mode
  :ensure nil
  :hook (text-mode
         . (lambda () (setopt show-trailing-whitespace t
		                     indicate-empty-lines t))))

;;; customization of display
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

;;; highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;;; auto show images
(use-package image-file
  :ensure nil
  :hook (after-init . auto-image-file-mode))

;;; auto revert outside changed file buffers
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;; save last visited point
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;; flymake
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;;; hide-show
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

;;; use-package select
(use-package select
  :ensure nil
  :custom
  (select-enable-clipboard t "enable clipboard"))

;;; !`TODO' test editorconfig mode
(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))

;;; save and switch window layouts
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

;;; dired
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-use-ls-dired nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (eq system-type 'darwin)
    (setopt insert-directory-program "gls")
    (setopt dired-use-ls-dired t))
  :bind (:map dired-mode-map
              ("TAB"       . dired-next-line)
              ("<backtab>" . dired-previous-line)))

;;; dictionary
(use-package dictionary
  :ensure nil
  :defer t
  :commands (dictionary-lookup-definition)
  :custom
  (dictionary-use-single-buffer t)
	(dictionary-server "dict.org")
  :bind ("M-#" . dictionary-lookup-definition))

;;; flyspell
(use-package flyspell
  :ensure nil
  :defer t
  :bind ((:map text-mode-map
               ("<f5>" . flyspell-mode))
         (:map prog-mode-map
               ("<f5>" . flyspell-prog-mode)))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; repeat mode
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

;;; so-long mode
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;;; parentheses
(use-package show-paren-mode
  :ensure nil
  :custom
  (show-paren-highlight-openparen t)
	(show-paren-style 'mixed)
	(show-paren-when-point-inside-paren t)
	(show-paren-when-point-in-periphery t)
	(show-paren-context-when-offscreen t)
  :hook (after-init . show-paren-mode))

;;; electric-pair-mode
(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-open-newline-between-pairs t)
  (electric-pair-skip-whitespace t)
  :hook (after-init . electric-pair-mode))

;;; abbrev-mode
(use-package abbrev
  :ensure nil
  :custom
  ;; (setq-default abbrev-mode nil)
  (save-abbrevs 'silently))

;;; which key
(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;; eglot mode: lsp
(use-package eglot
  :ensure nil
  :defer t
  ;; :custom
  ;; (eglot-autoshutdown t)
  ;; (eglot-confirm-server-initiated-edits nil)
  :bind
  ("C-c e r" . eglot-reconnect)
  ("C-c e s" . eglot-ensure)
  ("C-c e f" . eglot-format)
  ("C-c e e" . eglot-code-actions))

;;; file management
(use-package recentf
  :ensure nil
  :defer t
  :custom (recentf-max-menu-items 30)
  :hook (after-init . recentf-mode))

;;; docview
(use-package doc-view
  :ensure nil
  :defer t
  :custom
  (doc-view-ghost-program "mupdf")
  (doc-view-continuous t)
  (doc-view-resolution 300)
  (doc-view-scale-internally t)
  :bind (:map doc-view-mode-map
              ("<wheel-up>"   . doc-view-previous-page)
              ("<wheel-down>" . doc-view-next-line-or-next-page))
  :hook (doc-view-mode . doc-view-hide-modeline-mode))

;;; remap the buffer view
(use-package ibuffer
  :ensure nil
  :defer t
  :bind
  ([remap list-buffers] . ibuffer-other-window))

;;; eshell
(use-package eshell
  :ensure nil
  :defer t
  ;; :custom
  ;; (eshell-prompt-regexp "^[⟩⟫] ")
  ;; (eshell-prompt-function 'my/eshell-prompt)
  :hook
  (eshell-mode
   . (lambda ()
		   (keymap-set
		    eshell-mode-map
		    "C-d"
		    (lambda () (interactive)
		      (kill-buffer (current-buffer))))))
  ;; :bind (:map eshell-mode-map
  ;;             ("C-h" . '(lambda () (message "C-d"))))
  )



;; email settings
(setopt )
(setopt send-mail-function 'smtpmail-send-it)

(use-package rmail
  :ensure nil
  :defer t
  :custom
  (rmail-preserve-inbox t))

;; (use-package smtpmail
;;   :ensure nil
;;   :defer t
;;   :init
;;   (setq smtpmail-smtp-user "yly1228@foxmail.com"
;;         smtpmail-smtp-server "smtp.qq.com"
;;         smtpmail-smtp-service 465
;;         smtpmail-stream-type 'ssl))



;;; additional configuration files
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(use-package init-ui :ensure nil)       ; ui settings
(use-package init-packages :ensure nil) ; load packages
(use-package init-keymaps :ensure nil)  ; keymaps
(use-package init-evil :ensure nil)     ; evil bindings
(use-package init-org :ensure nil)      ; org mode settings
(use-package init-lang :ensure nil)     ; programming languages

;;; my custom lisp library(s)
(when (file-directory-p "~/Projects/ELisp")
  (message "We have user libs!")
  (add-to-list 'load-path "~/Projects/ELisp")

  (use-package escvil
    :disabled
    :ensure nil
    :defer t
    :hook ((prog-mode text-mode) . escvil-mode))

  (use-package handy-evil
    :ensure nil
    :defer t
    :hook ((prog-mode text-mode) . handy-evil-mode)))

;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; End:
;;; init.el ends here.
