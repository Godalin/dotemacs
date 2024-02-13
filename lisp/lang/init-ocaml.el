;;; package --- Initiation for OCaml Language
;;; Commentary:
;;; Code:


(use-package tuareg
  :defer t)


;; lsp intergation
(use-package merlin
  :defer t
  :after tuareg
  :hook
  (tuareg-mode . merlin-mode)
  (caml-mode . merlin-mode))


;; merlin & company
(use-package merlin-company
	:after (merlin company)
  :defer t)


;; easy opam switch
(use-package opam-switch-mode
  :ensure t
  :defer t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))


;; formatter
(use-package ocamlformat
	:defer t)


;; dune config files
(use-package dune
	:defer t)


(provide 'init-ocaml)
;;; init-ocaml.el ends here
