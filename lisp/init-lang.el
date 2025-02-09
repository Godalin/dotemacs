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
;; (use-package init-clojure :ensure nil)


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



;;; coq
(use-package proof-general
  :commands (proof-assert-next-command-interactive
	           proof-undo-last-successful-command
	           proof-goto-point)
  :custom
  (proof-electric-terminator-enable nil)
  (proof-toolbar-enable t)
  (PA-script-indent t)
  (proof-follow-mode 'followdown)
  :config
  (eval-after-load "proof-script"
    '(progn
       (keymap-set 'proof-mode-map "C-M-<down>"
                   #'proof-assert-next-command-interactive)
       (keymap-set 'proof-mode-map "C-M-<up>"
                   #'proof-undo-last-successful-command)
       (keymap-set 'proof-mode-map "C-M-<right>"
                   #'proof-goto-point)
       ))
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
   ("g" . #'keyboard-quit))
  :hook
  (coq-mode
   . (lambda ()                                ; prepare the coq mode
		   (opam-switch-set-switch "coq-env") ; switch to a good coq-env
		   (company-coq-mode t)               ; enable company-coq-mode
       (setq-local tab-always-indent nil)
       ))
  )

(use-package company-coq
  :defer t
  :after (opam-switch-mode proof-general)
  :hook
  (coq-mode . company-coq-mode)
  (company-coq-mode
   . (lambda ()
       (add-to-list 'company-coq-disabled-features 'prettify-symbols))))



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



(provide 'init-lang)


;;; init-lang.el ends here
