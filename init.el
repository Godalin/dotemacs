;;; package --- init.el
;;; Commentary:
;;; Code:


;; custom file
(setq custom-file
			(expand-file-name "custom.el" user-emacs-directory))


;; use the use-package package
(use-package use-package
	:init
	(setq use-package-always-ensure t))


;; use-package deal with package
(use-package package
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


;; yes or no
(use-package emacs
	:config
	(defalias 'yes-or-no-p 'y-or-n-p)
	(setq confirm-kill-processes nil))


;; theme
(use-package emacs
  :init
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mode-line
				'(accented 3d borderless 4 0.9))
  (setq modus-themes-hl-line
				'(accented))
  (setq modus-themes-region
				'(accented bg-only no-extend))
  (setq modus-themes-headings
				'((0 . (background overline rainbow 2.0))
					(1 . (background overline rainbow 1.5))
          (2 . (background overline 1.3))
          (t . (overline semibold))))
  :config
  (load-theme 'modus-vivendi)
  :bind
  ("<f12>" . 'modus-themes-toggle))


;; startup options

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Welcome to Godalin's Emacs\n\n")
(pixel-scroll-precision-mode)
(setq x-select-enable-clipboard-manager t)


;; fonts (faces)
(set-face-attribute 'default nil
                    :family "Julia Mono"
                    :height 160
                    :weight 'normal
                    :width  'normal)
(set-fontset-font "fontset-default" 'han "LXGW Wenkai")
(set-fontset-font "fontset-default" 'symbol "FontAwesome")


;; terminal face
(defface terminal
  '((t :family "GoMono Nerd Font"))
  "Face for vterm.")

(add-hook 'term-mode-hook
					(lambda ()
						(set (make-local-variable 'buffer-face-mode-face) 'terminal)
						(buffer-face-mode)))


;; input method
(setq read-quoted-char-radix 16)				;hex input as default

;; move through visual-lines / locigal-lines
(setq line-move-visual t)
(setq track-eol t)
(setq next-line-add-newlines nil)


;; display

(tab-bar-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)


;; set mode line
(size-indication-mode)			; size of buffer
(line-number-mode)			; line number
(column-number-mode)			; column number
(setq display-time-24hr-format t)
(setq display-time-mail-icon t)
(display-time-mode)			;time
(display-battery-mode)			;battery

(setq-default default-mode-line-color
	      (cons (face-background 'mode-line)
		    (face-foreground 'mode-line)))

(add-hook 'post-command-hook
	  (lambda ()
	    (let ((color (cond
			  ((minibufferp) '("#f5426f" . "#66ccff"))
			  ((bound-and-true-p evil-normal-state-p) '("#66ccff" . "#000000"))
			  ((bound-and-true-p evil-insert-state-p) '("#9cf542" . "#000000"))
			  ((bound-and-true-p evil-visual-state-p) '("#473ce6" . "#ffffff"))
			  ((bound-and-true-p evil-replace-state-p) '("#8e8f94" . "#ffffff"))
			  ((bound-and-true-p evil-operator-state-p) '("#8e8f94" . "#ffffff"))
			  ;; ((bound-and-true-p 'evil-motion-state-p) '("#c56de8" . "#ffffff"))
			  (t default-mode-line-color))))
	      (set-face-background 'mode-line (car color))
	      (set-face-foreground 'mode-line (cdr color)))))

;; set header line
(setq-default header-line-format
							`(""
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


(global-display-line-numbers-mode)
(global-hl-line-mode)
(global-auto-revert-mode)
(auto-save-visited-mode)
(auto-image-file-mode)


;; programming mode

;; edit parens (lisp code)
(setq-default tab-width 2)
(show-paren-mode)		    ;show pairs
(setq show-paren-style 'expression) ;in the form of the whole expression
(electric-pair-mode)		    ;auto insert closing ones


;; programming mode hooks
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)


;; set abbrev mode
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)


;; set eglot mode: lsp
(use-package eglot
  :bind
  (("C-c e s" . 'eglot-ensure)
   ("C-c e f" . 'eglot-format)
   ("C-c e e" . 'eglot-code-actions)))


;; file management
;; no backup files
(setq make-backup-files nil)
(add-hook 'before-save-hook
	  (lambda ()
	    (delete-trailing-whitespace)
	    (if (eglot-managed-p)
		(eglot-format))
	    (shell-command "fcitx5-remote -c")))


;; eshell
(use-package eshell
  :init
  ;; test whether
  (defun test-end-of-buffer ()
    "Test if point is at end of the buffer."
    (interactive)
    (= (point)
       (+ 1 (buffer-size))))

  ;; set face util
  (defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

  ;; the prompt
  (setq eshell-prompt-regexp "^[⟩⟫] ")
  (setq eshell-prompt-function
	(lambda ()
	  (concat
	   ;; begin
	   "⟫ "
	   ;; username
	   (with-face
	    (concat (user-login-name)
		    " ⟩ ")
	    :foreground "orange")
	   ;; path
	   (with-face
	    (concat (let ((pwd (eshell/pwd))
			  (home (getenv "HOME")))
		      (if (string-prefix-p home pwd)
			  (concat "~" (substring pwd (length home)))
			pwd))
		    " ⟩ ")
	    :foreground "red")
	   ;; time
	   (with-face
	    (format-time-string "♥ %H:%M ⟩ " (current-time))
	    :foreground "#66ccff")
	   ;; newline
	   "\n"
	   ;; character
	   (if (= (user-uid) 0) "⟩ " "⟫ ")))))


;; email settings
(setq user-mail-address "yly1228@foxmail.com")
(setq send-mail-function 'smtpmail-send-it)
(use-package smtpmail
  :init
  (setq smtpmail-smtp-user "yly1228@foxmail.com"
	smtpmail-smtp-server "smtp.qq.com"
	smtpmail-smtp-service 465
	smtpmail-stream-type 'ssl))


;; load packages
(require 'init-packages "~/.emacs.d/lisp/init-packages.el")

;; load keymaps & evil
(require 'init-keymaps "~/.emacs.d/lisp/init-keymaps.el")
(require 'init-evil "~/.emacs.d/lisp/init-evil.el")

;; load org-mode
(require 'init-org "~/.emacs.d/lisp/init-org.el")

;; load other languages
(require 'init-haskell "~/.emacs.d/lisp/lang/init-haskell.el")
(require 'init-racket "~/.emacs.d/lisp/lang/init-racket.el")
(require 'init-tex "~/.emacs.d/lisp/lang/init-tex.el")
(require 'init-sml "~/.emacs.d/lisp/lang/init-sml.el")
(require 'init-ocaml "~/.emacs.d/lisp/lang/init-ocaml.el")

;; agda mode
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; choose between editor and wm
(if (not (shell-command-to-string "wmctrl -m | grep LG3D"))
    (load "init-exwm.el"))


(provide 'init)
;;; init.el ends here
