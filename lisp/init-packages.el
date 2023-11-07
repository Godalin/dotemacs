;;; package --- This is module manages the packages that are not builtin with GNU Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;; best term
(use-package vterm
  :disabled t
  :ensure t
  :bind
  (:map
   vterm-mode-map
   ("C-q" . 'vterm-send-next-key))
  :hook
  (vterm-mode . (lambda ()
		  (set (make-local-variable 'buffer-face-mode-face) 'terminal)
		  (buffer-face-mode))))


;; which key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))


;; keycast
(use-package keycast
  :ensure t
  :demand t
  :init
  (keycast-tab-bar-mode))


;; company
(use-package company
  :ensure t
  :demand t
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3))
	company-tooltip-align-annotations t
	company-tooltip-margin 2)
  :hook
  (
   (after-init . global-company-mode)
   (haskell-mode . (lambda ()
		     (set (make-local-variable 'company-backends)
			  (append '((company-capf company-dabbrev-code))
				  company-backends)))))
  :bind
  (:map
   company-active-map
   ("RET" . 'newline-and-indent)
   ([return] . 'newline-and-indent)
   ("TAB" . company-complete-selection)
   ([tab] . company-complete-selection))
  )


;; format-all (not completed)
(use-package format-all
  :disabled
  :ensure t
  :defer t)


;; ivy-counsel-swiper completion
(use-package counsel
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d)")
  :config
  (ivy-mode 1)
  :bind
  ("C-s" . 'swiper-isearch)
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("M-y" . 'counsel-yank-pop)
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> i" . 'counsel-info-lookup-symbol)
  ("<f2> u" . 'counsel-unicode-char)
  ("<f2> j" . 'counsel-set-variable)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-c v" . 'ivy-push-view)
  ("C-c V" . 'ivy-pop-view)
  )


;;; Programming Languages

;; snippet
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode))
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


;; julia
(use-package julia-mode
  :ensure t
  :defer t)


;; coq
(use-package proof-general
  :ensure t
  :defer t)
(use-package company-coq
  :ensure t
  :defer t
  :hook (coq-mode . company-coq-mode))


;; common lisp
(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))


;; edit parentheses
(use-package paredit
  :ensure t
  :bind
  ("<f9>" . paredit-mode))


;; magit
(use-package magit
  :ensure t
  :init
  (setq magit-view-git-manual-method 'woman))


(provide 'init-packages)
;;; init-packages.el ends here
