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


;;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))


;;; markdown
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;;; common lisp mode
(add-hook 'lisp-mode-hook
          (lambda ()
            (load (expand-file-name "~/.quicklisp/slime-helper.el"))
            (setq inferior-lisp-program "sbcl")))


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


(use-package typst-ts-mode
  :ensure t
  :vc (typst-ts-mode
       :url "https://codeberg.org/meow_king/typst-ts-mode.git"))


(provide 'init-lang)


;;; init-lang.el ends here
