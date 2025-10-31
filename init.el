;;; package --- init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; custom file
(use-package emacs
  :init
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'no-error 'no-message))

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
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(setopt use-short-answers t)
(setopt confirm-kill-processes nil)    ; auto kill processes when exit
(setopt inhibit-startup-screen nil)    ; startup options
(setopt initial-scratch-message
        ";;; Welcome to Godalin's Emacs  -*- lexical-binding: t; -*-\n\n")

;;; Xia Wu WenKai
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default" 'han "LXGW Wenkai"))

(setopt read-quoted-char-radix 16)      ; input method, use hex code

;;; space/tab/indention related
(setopt tab-width 2)                   ; 2 spaces = 1 tab
(setopt tab-always-indent t)           ; only indent at left
(setopt indent-tabs-mode nil)          ; do not use tabs for indention
(setq indent-line-function #'tab-to-tab-stop) ; use a trivial indention function

;;; display
(add-hook 'after-init-hook #'tab-bar-mode)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(setopt scroll-preserve-screen-position t) ; remember point positions

(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'after-init-hook #'line-number-mode)
(add-hook 'after-init-hook #'column-number-mode)

(setopt make-backup-files nil)          ; do not create backup files

;;; white-space and indention
(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t
                      indicate-empty-lines t)))
(add-hook 'text-mode-hook
          (lambda () (setq show-trailing-whitespace t
		                  indicate-empty-lines t)))

;;; visual line mode
(use-package simple
  :ensure nil
  :custom
  (line-move-visual t)
  (track-eol t)
  (visual-line-fringe-indicators t)
  (word-wrap-by-category t))

(add-hook 'prog-mode-hook #'global-visual-line-mode)
(add-hook 'text-mode-hook #'global-visual-line-mode)

;;; customization of display
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'auto-save-visited-mode)
(add-hook 'after-init-hook #'auto-image-file-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'save-place-mode)


;;; fido-vertical
;; (add-hook 'after-init-hook #'fido-vertical-mode)

;;; flymake
(add-hook 'prog-mode-hook #'flymake-mode)

;;; hide-show
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

;;; use-package select
(setq select-enable-clipboard t)        ; enable clipboard

;;; editorconfig mode
(add-hook 'emacs-startup-hook #'editorconfig-mode)

; save and switch window layouts
(add-hook 'after-init-hook #'winner-mode)

;;; dired
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches
   "-aBhl --group-directories-first")
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
  :custom ((show-paren-highlight-openparen t)
	         (show-paren-style 'mixed)
	         (show-paren-when-point-inside-paren t)
	         (show-paren-when-point-in-periphery t)
	         (show-paren-context-when-offscreen t))
  :hook
  (after-init . show-paren-mode))

;;; electric-pair-mode
(use-package electric-pair-mode
  :ensure nil
  :init
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-delete-adjacent-pairs t)
  (setq electric-pair-open-newline-between-pairs t)
  (setq electric-pair-skip-whitespace t)
  :hook
  (emacs-startup . electric-pair-mode))

;;; abbrev-mode
(use-package abbrev
  :ensure nil
  :custom
  ;; (setq-default abbrev-mode nil)
  (save-abbrevs 'silently))

;; which key
(use-package which-key
  :ensure nil
  :hook (after-init-hook which-key-mode))

;;; set eglot mode: lsp
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



;; (defun my/eshell-prompt ()
;;   "The prompt for eshell."
;;   (concat
;;    ;; begin
;;    "⟫ "
;;    ;; username
;;    (my/with-face
;;     (concat (user-login-name) " ⟩ ")
;;     :foreground "orange")
;;    ;; path
;;    (my/with-face
;;     (concat (let ((pwd (eshell/pwd))
;;                   (home (getenv "HOME")))
;;               (if (string-prefix-p home pwd)
;;                   (concat "~" (substring pwd (length home)))
;;                 pwd))
;;             " ⟩ ")
;;     :foreground "red")
;;    ;; time
;;    (my/with-face
;;     (format-time-string "♥ %H:%M ⟩" (current-time))
;;     :foreground "#66ccff")
;;    ;; newline
;;    "\n"
;;    ;; character
;;    (if (= (user-uid) 0) "⟩ " "⟫ ")))

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
;; (setq user-mail-address "yly1228@foxmail.com")
;; (setq send-mail-function 'smtpmail-send-it)
;; (use-package smtpmail
;;   :ensure nil
;;   :defer t
;;   :init
;;   (setq smtpmail-smtp-user "yly1228@foxmail.com"
;;         smtpmail-smtp-server "smtp.qq.com"
;;         smtpmail-smtp-service 465
;;         smtpmail-stream-type 'ssl))



;;; Other files

;; add additional config path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(use-package init-ui :ensure nil)       ; ui settings
(use-package init-packages :ensure nil) ; load packages
(use-package init-keymaps :ensure nil)  ; keymaps
(use-package init-evil :ensure nil)     ; evil bindings
(use-package init-org :ensure nil)      ; org mode settings
(use-package init-lang :ensure nil)     ; programming languages



;; my custom lisp library(s)
(when (file-directory-p "~/Projects/ELisp")
  (message "We have user libs!")
  (add-to-list 'load-path "~/Projects/ELisp")

  (use-package escvil
    :disabled
    :ensure nil
    :commands escvil-mode
    :defer t
    :hook
    (prog-mode . escvil-mode)
    (text-mode . escvil-mode))

  (use-package handy-evil
    :ensure nil
    :commands handy-evil-mode
    :defer t
    :hook
    (prog-mode . handy-evil-mode)
    (text-mode . handy-evil-mode)))

;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; End:
;;; init.el ends here.
