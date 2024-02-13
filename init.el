;;; package --- init.el
;;; Commentary:
;;; Code:


;; custom file
(setq custom-file
			(expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)


;; use the use-package package
(use-package use-package
	:ensure nil
	:init
	(setq use-package-always-ensure t))


;; use-package deal with package
(use-package package
	:ensure nil
  :config
  (setq package-archives
				'(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
					("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
					("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

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
				";; Welcome to Godalin's Emacs\n\n")
	(setq x-select-enable-clipboard-manager t))


;; faces
;; (set-face-attribute 'default nil
;;                     :family "Julia Mono"
;;                     :height 160
;;                     :weight 'normal
;;                     :width  'normal)
(set-fontset-font "fontset-default" 'han "LXGW Wenkai")
(set-fontset-font "fontset-default" 'symbol "FontAwesome")


;; terminal face
(defface terminal
  '((t :family "GoMono Nerd Font"))
  "Face for vterm.")


(defun my/set-term-font ()
	"Set good fonts for terminal modes."
	(interactive)
	(set (make-local-variable 'buffer-face-mode-face) 'terminal)
	(buffer-face-mode))


(use-package term
	:ensure nil
	:hook
	(term-mode . my/set-term-font)
	:bind
	(:map
	 term-mode-map
	 ("C-d" . (lambda () (interactive)
							(term-handle-exit)
							(kill-buffer)))))


;; input method
(use-package emacs
	:custom
	(read-quoted-char-radix 16))


;; winner
(use-package winner-mode
	:ensure nil
	:hook
	(after-init . winner-mode))


;; dired
(use-package dired
	:ensure nil
	:custom
	(dired-listing-switches "-aBhl --group-directories-first")
	(dired-kill-when-opening-new-dired-buffer t))


(use-package repeat
	:ensure nil
	:hook
	(after-init . repeat-mode))


;; use visual lines
(use-package emacs
	:ensure nil
	:init
	(setq line-move-visual t)
	(setq track-eol t)
	:hook
	(after-init . global-visual-line-mode))


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
(setq-default header-line-format
							`("%e"
								mode-line-front-space
								"Welcome to Emacs"
								mode-line-end-spaces
								))


;; scroll
(setq scroll-preserve-screen-position t)

;; white-space and indention
(add-hook 'prog-mode-hook
					(lambda () (setq show-trailing-whitespace t
											indicate-empty-lines t)))
(add-hook 'text-mode-hook
					(lambda () (setq show-trailing-whitespace t
											indicate-empty-lines t)))

(use-package emacs
	:ensure nil
	:custom
	(global-display-line-numbers-type 'relative)
	:hook
	(after-init . global-display-line-numbers-mode)
	(after-init . global-hl-line-mode)
	(after-init . auto-save-visited-mode)
	(after-init . auto-image-file-mode)
	(after-init . global-auto-revert-mode)
	(after-init . save-place-mode)
	;; (after-init . fido-vertical-mode)
	)


(use-package so-long
  :ensure nil
  :hook
	(after-init . global-so-long-mode))


;; programming mode
(setq-default tab-width 2)


;; edit parens (lisp code)
(use-package emacs
	:ensure nil
	:init
	(setq show-paren-style 'expression)
	:hook
	(after-init . show-paren-mode)
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
	(setq-default abbrev-mode t)
	(setq save-abbrevs 'silently))


;; set eglot mode: lsp
(use-package eglot
	:ensure nil
  :bind
  ("C-c e s" . 'eglot-ensure)
  ("C-c e f" . 'eglot-format)
  ("C-c e e" . 'eglot-code-actions))


;; tree sitter
(use-package treesit-auto
	:custom
	(treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))


;; file management
(use-package recentf
	:ensure nil
	:custom
	(recentf-max-menu-items 10)
	:hook
	(after-init . recentf-mode))


(setq make-backup-files nil)


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
									 (keymap-set eshell-mode-map "C-d"
																	 (lambda () (interactive)
																		 (eshell-return-to-prompt)
																		 (end-of-buffer)
																		 (eshell-kill-input)
																		 (insert "exit")
																		 (eshell-send-input)
																		 (message "eshell")))
									 (display-line-numbers-mode -1))))


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


;; editor or wm
(if (not (shell-command-to-string "wmctrl -m | grep LG3D"))
    (load "init-exwm.el"))


(provide 'init)
;;; init.el ends here
