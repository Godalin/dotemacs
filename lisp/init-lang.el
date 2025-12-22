;;; package --- init-lang.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp/lang" user-emacs-directory))

(require 'init-haskell)
(require 'init-racket)
(require 'init-tex)
(require 'init-sml)
(require 'init-ocaml)
(require 'init-coq)
(require 'init-nix)



;;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))



;;; Markdown ↓
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-block-natively t))



;;; elixir mode
(use-package elixir-mode
  :defer t)



;;; prolog mode
(use-package prolog-mode
  :ensure nil
  :defer t
  :after ediprolog
  :custom
  (prolog-electric-if-then-else-flag t)

  :config
  (define-skeleton my/prolog-load
    "Insert a load statement of prolog."
    "" ":- use_module(library(" _ ")).")

  :bind ( :map prolog-mode-map
          ("C-c l" . my/prolog-load)
          ("<f10>" . ediprolog-dwim)))

(use-package ediprolog
  :defer t)



;;; typst
(use-package typst-ts-mode
  :defer t
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git"
        :rev :newest))



;;; lean
(use-package nael
  :defer t
  :after eglot
  :hook (nael-mode . eglot-ensure))



(provide 'init-lang)

;;; init-lang.el ends here
