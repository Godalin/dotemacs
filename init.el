;;; package --- init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; use `use-package' to use `use-package' package
(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure t)
  (use-package-always-defer nil)
  (use-package-enable-imenu-support t))

;;; use `use-package' to use `package'
(use-package package
  :ensure nil
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

;;; package-install-vc
(use-package package-vc
  :ensure nil
  :custom
  (package-vc-allow-build-commands t))

;;; setup `benchmark-init'
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; `dimish' for the minor modes in mod line
(use-package diminish)

;;; load environment variables
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;;; custom file
(use-package emacs
  :ensure nil
  :init
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
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
  :ensure nil
  :defer 10
  :custom
  (confirm-kill-processes nil "auto kill processes when exit")
  (make-backup-files nil "do not create backup files")
  :config
  (auto-save-visited-mode))

;;; Xia Wu WenKai
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default" 'han "LXGW Wenkai"))

;;; space/tab/indention related
(use-package emacs
  :ensure nil
  :custom
  (tab-width         2   "2 spaces = 1 tab")
  (tab-always-indent nil "only indent at left")
  (indent-tabs-mode  nil "no tabs for indentation"))

;;; visual line mode and more
(use-package simple
  :ensure nil
  :diminish visual-line-mode
  :custom
  (read-quoted-char-radix 16)
  (track-eol t)
  ;; visual line settings
  (line-move-visual t)
  (visual-line-fringe-indicators t)
  (word-wrap-by-category t)
  :init
  (line-number-mode -1)
  (column-number-mode -1)
  (size-indication-mode -1)
  :hook ((after-init . size-indication-mode)
         ;; visual line in text edit modes
         ((prog-mode text-mode) . visual-line-mode)))

;;; use tab-bar to show something
;; (use-package tab-bar
;;   :ensure nil
;;   :hook (after-init . tab-bar-mode))

;;; better scroll
(use-package pixel-scroll
  :ensure nil
  :defer 10
  :config
  (pixel-scroll-precision-mode))

;;; prog-mode
(use-package prog-mode
  :ensure nil
  :hook ((emacs-lisp-mode . prettify-symbols-mode)
         (prog-mode
          . (lambda () (setq-local show-trailing-space t
                              indicate-empty-lines t)))))
;;; text-mode
(use-package text-mode
  :ensure nil
  :hook (text-mode
         . (lambda () (setq-local show-trailing-whitespace t
                             indicate-empty-lines t))))

;;; customization of display
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode text-mode)
         . display-line-numbers-mode))

;;; highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;;; auto show images
(use-package image-file
  :ensure nil
  :defer 10
  :config
  (auto-image-file-mode))

;;; auto revert outside changed file buffers
(use-package autorevert
  :ensure nil
  :defer 10
  :config
  (global-auto-revert-mode))

;;; save last visited point
(use-package saveplace
  :ensure nil
  :defer 10
  :config
  (save-place-mode))

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

;;; editorconfig mode
(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))

;;; save and switch window layouts
(use-package winner
  :ensure nil
  :defer 10
  :config
  (winner-mode))

;;; flymake
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind ( :map flymake-mode-map
          ("M-p" . flymake-goto-prev-error)
          ("M-n" . flymake-goto-next-error)))

;;; eldoc mode
(use-package eldoc
  :ensure nil
  :defer 5
  :diminish eldoc-mode
  :config
  (global-eldoc-mode))

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
  :bind ( :map dired-mode-map
          ("TAB"       . dired-next-line)
          ("<backtab>" . dired-previous-line)))

;;; which function
;; (use-package which-func
;;   :ensure nil
;;   :custom
;;   (which-func-display 'header)
;;   :hook (after-init . which-function-mode))

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
  :diminish flyspell-mode
  :bind (( :map text-mode-map
           ("<f5>" . flyspell-mode))
         ( :map prog-mode-map
           ("<f5>" . flyspell-prog-mode)))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; repeat mode
(use-package repeat
  :ensure nil
  :defer 10
  :config
  (repeat-mode))

;;; so-long mode
(use-package so-long
  :ensure nil
  :defer 10
  :config
  (global-so-long-mode))

;;; parentheses
(use-package show-paren-mode
  :ensure nil
  :defer 10
  :custom
  (show-paren-highlight-openparen t)
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen t)
  :config
  (show-paren-mode))

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
  (abbrev-suggest t)
  ;; (setq-default abbrev-mode nil)
  (save-abbrevs 'silently))

;;; which key
(use-package which-key
  :ensure nil
  :defer 10
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; eglot mode: lsp
(use-package eglot
  :ensure nil
  :defer t
  :bind (("C-c e r" . eglot-reconnect)
         ("C-c e s" . eglot-ensure)
         ("C-c e f" . eglot-format)
         ("C-c e e" . eglot-code-actions)))

;;; file management
(use-package recentf
  :ensure nil
  :defer t
  :custom (recentf-max-menu-items 30)
  :hook (after-init . recentf-mode))

;;; doc-view
(use-package doc-view
  :ensure nil
  :defer t
  :custom
  (doc-view-ghost-program "mupdf")
  (doc-view-continuous t)
  (doc-view-resolution 300)
  (doc-view-scale-internally t)
  :bind ( :map doc-view-mode-map
          ("<wheel-up>"   . doc-view-previous-page)
          ("<wheel-down>" . doc-view-next-line-or-next-page))
  :hook (doc-view-mode . doc-view-hide-modeline-mode))

;;; remap the buffer view
(use-package ibuffer
  :ensure nil
  :defer t
  :bind ([remap list-buffers] . ibuffer-other-window))

;;; eshell
(use-package eshell
  :ensure nil
  :defer 10
  :config
  (add-to-list 'eshell-modules-list
               'eshell-rebind))

;;; outlines
(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :hook (emacs-lisp-mode . outline-minor-mode))



;;; Custom:
;;; additional configuration files
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;;; User Configuration Files

;;; ui settings
(require 'init-ui)

;;; my fancy keymaps
(require 'init-keymaps)

;;; load packages
(require 'init-packages)

;;; evil bindings
(require 'init-evil)

(require 'init-org)                     ; org mode settings
(require 'init-lang)                    ; programming languages

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
    :disabled
    :ensure nil
    :defer t
    :hook ((prog-mode text-mode) . handy-evil-mode)))

;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; End:
;;; init.el ends here.
