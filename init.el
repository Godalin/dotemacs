;;; package --- init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; custom file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)


;; use the use-package package
(use-package use-package
  :ensure nil
  :custom ((use-package-always-ensure t "Default :ensure to t in `use-package'.")
					 (use-package-enable-imenu-support t)))


;; use-package deal with package
(use-package package
  :ensure nil
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))

  ;; if not initialized then initialize it
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (unless package-archive-contents
    (package-refresh-contents)))


;; vc use-package, will be removed after emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package.git"))
(use-package vc-use-package
  :vc (:fetcher github :repo slotThe/vc-use-package))


;; yes or no
(use-package emacs
  :ensure nil
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-processes nil))


;; startup options
(use-package emacs
  :ensure nil
  :init
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message
        ";;; Welcome to Godalin's Emacs  -*- lexical-binding: t; -*-\n\n")
  (setq x-select-enable-clipboard-manager t))



(set-fontset-font "fontset-default" 'han "LXGW Wenkai")
(set-fontset-font "fontset-default" 'symbol "FontAwesome")


(defun my/set-term-font ()
  "Set good fonts for terminal modes."
  (interactive)
  (set (make-local-variable 'buffer-face-mode-face) 'terminal)
  (buffer-face-mode))


;; term mode
(use-package term
  :ensure nil
	:custom-face
	(terminal ((t :family "GoMono Nerd Font")))
  :hook
  (term-mode . my/set-term-font)
  :bind
  (:map
   term-mode-map
   ("C-c C-d" . (lambda () (interactive)
									(kill-buffer (current-buffer))))))


;; input method
(use-package emacs
  :custom
  (read-quoted-char-radix 16))


;; winner
(use-package winner-mode
  :ensure nil
  :hook
  (after-init . winner-mode))


(use-package emacs
  :disabled
  :ensure nil
  :custom
  (split-height-threshold nil)
  (split-width-threshold 0))


;; dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t))


;; dictionary
(use-package dictionary
	:ensure nil
	:custom ((dictionary-use-single-buffer t)
					 (dictionary-server "localhost"))
	:bind (("M-#" . #'dictionary-lookup-definition)))


;; repeat mode
(use-package repeat
  :ensure nil
  :hook
  (after-init . repeat-mode))


;; display
(use-package emacs
  :ensure nil
  :hook
  (after-init . tab-bar-mode)
  (after-init . pixel-scroll-precision-mode))


;; mode line info
(use-package emacs
  :ensure nil
  :custom
  (display-time-24hr-format t)
  (display-time-mail-icon t)
  :hook
  (after-init . size-indication-mode)
  (after-init . line-number-mode)
  (after-init . column-number-mode))


;; set header line
;; (setq-default header-line-format
;;                `("%e"
;;                  mode-line-front-space
;;                  "Welcome to Emacs"
;;                  mode-line-end-spaces
;;                  ))


;; scroll
(setq scroll-preserve-screen-position t)

;; white-space and indention
(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t
                      indicate-empty-lines t)))
(add-hook 'text-mode-hook
          (lambda () (setq show-trailing-whitespace t
                      indicate-empty-lines t)))


;; visual line mode
(use-package emacs
  :ensure nil
  :custom
  (line-move-visual t)
  (track-eol t)
  (visual-line-fringe-indicators t)
  (word-wrap-by-category t)
  :hook
  (prog-mode . global-visual-line-mode)
	(text-mode . global-visual-line-mode))


;; customization of display
(use-package emacs
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (after-init . global-hl-line-mode)
  (after-init . auto-save-visited-mode)
  (after-init . auto-image-file-mode)
  (after-init . global-auto-revert-mode)
  (after-init . save-place-mode)
  (prog-mode . display-line-numbers-mode)

  ;; (after-init . fido-vertical-mode)
  )


;; so long mode
(use-package so-long
  :ensure nil
  :hook
  (after-init . global-so-long-mode))


;; tab related
(use-package emacs
  :ensure nil
  :custom
  (tab-width 2)
	(indent-tabs-mode nil))


;; parentheses
(use-package show-paren-mode
  :ensure nil
	:custom ((show-paren-highlight-openparen t)
					 (show-paren-style 'mixed)
					 (show-paren-when-point-inside-paren t)
					 (show-paren-when-point-in-periphery t)
					 (show-paren-context-when-offscreen t))
  :hook
	(after-init . show-paren-mode))

(use-package electric-pair-mode
	:ensure nil
	:custom
	(electric-pair-preserve-balance t)
	(electric-pair-delete-adjacent-pairs t)
	(electric-pair-open-newline-between-pairs t)
	(electric-pair-skip-whitespace t)
	:hook
	(after-init . electric-pair-mode))


;; programming mode hooks
(use-package emacs
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  (prog-mode . hs-minor-mode)
  (prog-mode . prettify-symbols-mode))


;; set abbrev mode
(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode nil)
  (setq save-abbrevs 'silently))


;; set eglot mode: lsp
(use-package eglot
  :ensure nil
  ;; :custom
  ;; (eglot-autoshutdown t)
  ;; (eglot-confirm-server-initiated-edits nil)
  :config
  (setq-default eglot-workspace-configuration
                '((:haskell
                   (:formattingProvider . "fourmolu"))))
  :bind
  ("C-c e r" . 'eglot-reconnect)
  ("C-c e s" . 'eglot-ensure)
  ("C-c e f" . 'eglot-format)
  ("C-c e e" . 'eglot-code-actions))


;; file management
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 30)
  :hook
  (after-init . recentf-mode))

(setq make-backup-files nil)


;; docview
(use-package doc-view
  :ensure nil
  :custom
  (doc-view-ghost-program "mupdf")
  (doc-view-continuous t)
  (doc-view-resolution 300)
  (doc-view-scale-internally t)
  :bind
  (:map
   doc-view-mode-map
   ("<wheel-up>" . 'doc-view-previous-page)
   ("<wheel-down>" . 'doc-view-next-line-or-next-page))
  :hook
  (doc-view-mode . doc-view-hide-modeline-mode))


(keymap-global-set "<remap> <list-buffers>" 'ibuffer-other-window)


(defun my/with-face (str &rest face-plist)
  "Add face to string."
  (propertize str 'face face-plist))


(defun my/eshell-prompt ()
  "The prompt for eshell."
  (concat
   ;; begin
   "⟫ "
   ;; username
   (my/with-face
    (concat (user-login-name) " ⟩ ")
    :foreground "orange")
   ;; path
   (my/with-face
    (concat (let ((pwd (eshell/pwd))
                  (home (getenv "HOME")))
              (if (string-prefix-p home pwd)
                  (concat "~" (substring pwd (length home)))
                pwd))
            " ⟩ ")
    :foreground "red")
   ;; time
   (my/with-face
    (format-time-string "♥ %H:%M ⟩" (current-time))
    :foreground "#66ccff")
   ;; newline
   "\n"
   ;; character
   (if (= (user-uid) 0) "⟩ " "⟫ ")))


;; eshell
(use-package eshell
  :ensure nil
  :custom
  (eshell-prompt-regexp "^[⟩⟫] ")
  (eshell-prompt-function 'my/eshell-prompt)
  :hook
  (eshell-mode . (lambda ()
									 (keymap-set
										eshell-mode-map
										"C-d"
										(lambda () (interactive)
											(kill-buffer (current-buffer)))))))


;; email settings
(setq user-mail-address "yly1228@foxmail.com")
(setq send-mail-function 'smtpmail-send-it)
(use-package smtpmail
  :ensure nil
  :init
  (setq smtpmail-smtp-user "yly1228@foxmail.com"
        smtpmail-smtp-server "smtp.qq.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl))


;;; Other files


;; add local config path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; ui settings
(use-package init-ui
  :ensure nil)

;; load packages
(use-package init-packages
  :ensure nil)

;; keymaps
(use-package init-keymaps
  :ensure nil)

;; evil bindings
(use-package init-evil
  :ensure nil)

;; org mode settings
(use-package init-org
  :ensure nil)

;; programming languages
(use-package init-lang
  :ensure nil)



;; my custom lisp library(s)
(add-to-list 'load-path "~/Projects/ELisp")
(use-package escvil
	:ensure nil
	:commands escvil-mode
	:defer t
	:hook
	(prog-mode . escvil-mode)
	(text-mode . escvil-mode))



;; editor or wm
;; (if (not window-system)
;; 		(progn
;; 			(use-package init-exwm :ensure nil)
;; 			(exwm-enable)))
