;;; package --- init-lang.el
;;; Commentary:
;;; Code:


(add-to-list 'load-path
						 (expand-file-name "lisp/lang" user-emacs-directory))


(use-package init-haskell :ensure nil)
(use-package init-racket :ensure nil)
(use-package init-tex :ensure nil)
(use-package init-sml :ensure nil)
(use-package init-ocaml :ensure nil)


;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))


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


;; elixir mode
(use-package elixir-mode
	:defer t)


;; lean-4
(use-package lean4-mode
	:defer t
	:vc (:fetcher github :repo leanprover/lean4-mode))


;; edit parentheses
(use-package paredit
  :bind
  ("<f9>" . paredit-mode))


;; kmonad kbd
(use-package kbd-mode
	:defer t
	:vc (:fetcher github :repo kmonad/kbd-mode)
	:custom
  (kbd-mode-kill-kmonad "pkill -9 kmonad")
  (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/best.kbd"))


;; typst ts mode
(use-package typst-ts-mode
	:defer t
	:vc (:fetcher sourcehut :repo "meow_king/typst-ts-mode")
	:custom
	(typst-ts-mode-watch-options "--open")
	(typst-ts-mode-indent-offset 2))


;; zig mode
(use-package zig-mode
	:defer t)


(provide 'init-lang)
;;; init-lang.el ends here
