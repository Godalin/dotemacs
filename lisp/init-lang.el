;;; package --- init-lang.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp/lang" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "lisp/lang/modes" user-emacs-directory))

(use-package init-haskell :ensure nil)
(use-package init-racket :ensure nil)
(use-package init-tex :ensure nil)
(use-package init-sml :ensure nil)

(use-package init-ocaml :ensure nil)
(use-package init-coq :ensure nil)

(use-package init-nix :ensure nil)



;;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))



;;; Markdown ↓
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))



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
  :bind
  (:map
   prolog-mode-map
   ("C-c l" . (lambda () (interactive)
                (skeleton-insert
                 '(nil ":- use_module(library(" _ "))."))))
   ("<f10>" . 'ediprolog-dwim)))

(use-package ediprolog
  :defer t)



;;; Typst
(use-package typst-ts-mode
  :ensure t
  :defer t
  :vc ( :url "https://codeberg.org/meow_king/typst-ts-mode.git"
        :rev :newest))



;;; Lean
(use-package nael
  :defer t
  :after eglot
  :vc ( :url "https://codeberg.org/mekeor/nael.git"
        :rev :newest
        :doc "nael/README.org"
        :lisp-dir "nael")
  :hook (nael-mode . eglot-ensure))

(use-package nael-markdown
  :after markdown-mode
  :defer t
  :vc ( :url "https://codeberg.org/mekeor/nael.git"
        :rev :newest
        :lisp-dir "nael-markdown"))



(provide 'init-lang)

;;; init-lang.el ends here
