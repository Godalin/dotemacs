;;; package --- init-lang.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(add-to-list 'load-path
						 (expand-file-name "lisp/lang" user-emacs-directory))


(use-package init-haskell :ensure nil)
(use-package init-racket :ensure nil)
(use-package init-tex :ensure nil)
(use-package init-sml :ensure nil)
(use-package init-ocaml :ensure nil)


;;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
			(append
			 '(("\\.agda\\'" . agda2-mode)
				 ("\\.lagda.md\\'" . agda2-mode))
			 auto-mode-alist))


;;; markdown
(use-package markdown-mode
	:defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;;; bqn
(use-package bqn-mode
	:defer t)


;;; julia
(use-package julia-mode
  :defer t)


;;; coq
(use-package proof-general
  :defer t)

(use-package company-coq
	:after proof-general
  :defer t
  :hook
	(coq-mode . company-coq-mode))


;;; common lisp
(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))


;;; scheme
(use-package scheme-mode
	:ensure nil
	:defer t
	:custom
	(scheme-program-name "chez"))

(use-package geiser-chez
	:defer t
	:custom
	(geiser-chez-binary "chez"))

(use-package geiser-guile
	:defer t)


;;; elixir mode
(use-package elixir-mode
	:defer t)


;;; prolog mode
(use-package prolog-mode
	:ensure nil
	:defer t
	:custom
	(prolog-electric-if-then-else-flag t)
	:bind
	(("<f10>" . 'ediprolog-dwim)
	 :map
	 prolog-mode-map
	 ("C-c l" . (lambda () (interactive)
								(skeleton-insert '(nil ":- use_module(library(" _ "))."))))))

(use-package ediprolog
	:defer t
	:after prolog-mode)


;;; lean-4
(use-package lean4-mode
	:defer t
	:vc (:url "https://github.com/leanprover/lean4-mode"))


;;; kmonad kbd
(use-package kbd-mode
	:defer t
	:vc (:url "https://github.com/kmonad/kbd-mode")
	:custom
  (kbd-mode-kill-kmonad "pkill -9 kmonad")
  (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/best.kbd"))


;;; typst ts mode
(use-package typst-ts-mode
	:defer t
	:vc (:url "https://git.sr.ht/~meow_king/typst-ts-mode")
	:custom
	(typst-ts-mode-watch-options "--open")
	(typst-ts-mode-indent-offset 2))


;;; zig mode
(use-package zig-mode
	:defer t)


(provide 'init-lang)


;;; init-lang.el ends here
