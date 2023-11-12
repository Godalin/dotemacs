;;; package --- init-lang.el
;;; Commentary:
;;; Code:


(add-to-list 'load-path
						 (expand-file-name "lisp/lang" user-emacs-directory))


(require 'init-haskell)
(require 'init-racket)
(require 'init-tex)
(require 'init-sml)
(require 'init-ocaml)


;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))


;; markdown
(use-package markdown-mode
  :ensure
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; bqn
(use-package bqn-mode)


;; julia
(use-package julia-mode
  :defer t)


;; coq
(use-package proof-general
  :defer t)
(use-package company-coq
	:after proof-general
  :defer t
  :hook (coq-mode . company-coq-mode))


;; common lisp
(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))


;; edit parentheses
(use-package paredit
  :bind
  ("<f9>" . paredit-mode))


(provide 'init-lang)
;;; init-lang.el ends here
