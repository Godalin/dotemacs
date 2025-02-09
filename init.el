;;; package --- init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; print emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)))

;;; custom file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)



;;; use the use-package package
(use-package use-package
  :ensure nil
  :init
  (setq use-package-always-ensure t
	      use-package-enable-imenu-support t))

;;; use-package deal with package
(use-package package
  :ensure nil
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))



(defalias 'yes-or-no-p 'y-or-n-p)       ; yes or no
(setq confirm-kill-processes nil)       ; auto kill processes when exit

(setq inhibit-startup-screen nil)       ; startup options
(setq initial-scratch-message
      ";;; Welcome to Godalin's Emacs  -*- lexical-binding: t; -*-\n\n")

(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default" 'han "LXGW Wenkai"))



(setq read-quoted-char-radix 16)        ; input method, use hex code



;;; space/tab related
(setq-default tab-width 2)             ; 2 spaces = 1 tab
(setq-default indent-tabs-mode nil)    ; do not use tabs for indention



;;; display
(add-hook 'after-init-hook #'tab-bar-mode)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(setq scroll-preserve-screen-position t) ; remember point positions

(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'after-init-hook #'line-number-mode)
(add-hook 'after-init-hook #'column-number-mode)

(setq make-backup-files nil)            ; do not create backup files

;;; white-space and indention
(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t
		                  indicate-empty-lines t)))
(add-hook 'text-mode-hook
          (lambda () (setq show-trailing-whitespace t
		                  indicate-empty-lines t)))

;;; visual line mode
(setq line-move-visual t)
(setq track-eol t)
(setq visual-line-fringe-indicators t)
(setq word-wrap-by-category t)
(add-hook 'prog-mode-hook #'global-visual-line-mode)
(add-hook 'text-mode-hook #'global-visual-line-mode)



;;; customization of display
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 'relative))

(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'auto-save-visited-mode)
(add-hook 'after-init-hook #'auto-image-file-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; (add-hook 'after-init-hook #'fido-vertical-mode)

;;; programming mode hooks
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

(setq select-enable-clipboard t)        ; enable clipboard



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

(defun my/set-term-font ()
  "Set good fonts for terminal modes."
  (interactive)
  (set (make-local-variable 'buffer-face-mode-face) 'terminal)
  (buffer-face-mode))



(add-hook 'after-init #'winner-mode)    ; save and switch window layouts


;;; dired
(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-listing-switches "-aBhl --group-directories-first"
        dired-use-ls-dired nil
        dired-kill-when-opening-new-dired-buffer t)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"
          dired-use-ls-dired t)))


;;; dictionary
(use-package dictionary
  :ensure nil
  :defer t
  :commands (dictionary-lookup-definition)
  :custom ((dictionary-use-single-buffer t)
	         (dictionary-server "localhost"))
  :bind
  ("M-#" . #'dictionary-lookup-definition))


;;; repeat mode
(use-package repeat
  :ensure nil
  :hook
  (after-init . repeat-mode))


(add-hook 'after-init-hook #'global-so-long-mode) ; so long mode



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

(use-package electric-pair-mode
  :ensure nil
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-open-newline-between-pairs t)
  (electric-pair-skip-whitespace t)
  :hook
  (after-init . electric-pair-mode))


(setq-default abbrev-mode nil)          ; set abbrev mode
(setq save-abbrevs 'silently)           ; silently save abbrevs


;;; set eglot mode: lsp
(use-package eglot
  :ensure nil
  :defer t
  ;; :custom
  ;; (eglot-autoshutdown t)
  ;; (eglot-confirm-server-initiated-edits nil)
  :commands (eglot-ensure
             eglot-reconnect
             eglot-format
             eglot-code-actions)
  :config
  (setq-default eglot-workspace-configuration
                '((:haskell
                   (:formattingProvider . "fourmolu"))))
  :bind
  ("C-c e r" . #'eglot-reconnect)
  ("C-c e s" . #'eglot-ensure)
  ("C-c e f" . #'eglot-format)
  ("C-c e e" . #'eglot-code-actions))


;;; file management
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 30)
  :hook
  (after-init . recentf-mode))

;;; docview
(use-package doc-view
  :ensure nil
  :defer t
  :commands (doc-view-previous-page
	           doc-view-next-line-or-next-page)
  :custom
  (doc-view-ghost-program "mupdf")
  (doc-view-continuous t)
  (doc-view-resolution 300)
  (doc-view-scale-internally t)
  :bind
  (:map
   doc-view-mode-map
   ("<wheel-up>" . #'doc-view-previous-page)
   ("<wheel-down>" . #'doc-view-next-line-or-next-page))
  :hook
  (doc-view-mode . doc-view-hide-modeline-mode))



;;; remap the buffer view
(keymap-global-set "<remap> <list-buffers>" 'ibuffer-other-window)



;; (defun my/with-face (str &rest face-plist)
;;   "Add face to string."
;;   (propertize str 'face face-plist))


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


;; eshell
;; (use-package eshell
;;   :ensure nil
;;   :custom
;;   (eshell-prompt-regexp "^[⟩⟫] ")
;;   (eshell-prompt-function 'my/eshell-prompt)
;;   :hook
;;   (eshell-mode . (lambda ()
;; 		   (keymap-set
;; 		    eshell-mode-map
;; 		    "C-d"
;; 		    (lambda () (interactive)
;; 		      (kill-buffer (current-buffer)))))))


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
;; (use-package init-evil :ensure nil)     ; evil bindings
;; (use-package init-org :ensure nil)      ; org mode settings
(use-package init-lang :ensure nil)     ; programming languages



;; my custom lisp library(s)
(when (file-directory-p "~/Projects/ELisp")
  (message "We have user libs!")
  (add-to-list 'load-path "~/Projects/ELisp")
  (use-package escvil
    :ensure nil
    :commands escvil-mode
    :defer t
    :hook
    (prog-mode . escvil-mode)
    (text-mode . escvil-mode))
  )

;;; init.el ends here.
