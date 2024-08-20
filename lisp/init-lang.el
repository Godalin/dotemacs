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
(use-package init-clojure :ensure nil)


;;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

;;; cubicaltt
(load-file "$HOME/Projects/cubicaltt/cubicaltt.el")
;; (autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
(add-to-list 'auto-mode-alist '("\\.ctt$" . cubicaltt-mode))

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
  :defer t
  :config
  (setq proof-electric-terminator-enable nil)
  :bind
  (:repeat-map
   coq-repeat-mode-map
   ("n" . #'proof-assert-next-command-interactive)
   ("p" . #'proof-undo-last-successful-command)
   ("u" . #'proof-undo-last-successful-command)
   ("C-n" . #'proof-assert-next-command-interactive)
   ("C-p" . #'proof-undo-last-successful-command)
   ("C-u" . #'proof-undo-last-successful-command)
   :exit
   ("g" . #'keyboard-quit)))

(use-package company-coq
  :ensure t
  :defer t
  :hook
  (coq-mode . #'company-coq-mode))


(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")


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


;; lean-4
(use-package lean4-mode
  :defer t
  :vc (:fetcher github :repo leanprover/lean4-mode))


;; kmonad kbd
;; (use-package kbd-mode
;;   :defer t
;;   :vc (:fetcher github :repo kmonad/kbd-mode)
;;   :custom
;;   (kbd-mode-kill-kmonad "pkill -9 kmonad")
;;   (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/best.kbd"))


;; typst ts mode
(use-package typst-ts-mode
  :defer t
  :vc (:fetcher sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-indent-offset 2))


;; LF twelf
(setq twelf-root "/home/godalin/Projects/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))


(provide 'init-lang)


;;; init-lang.el ends here
