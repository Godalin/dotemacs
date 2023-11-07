;;; package --- Initiation of Ocaml Language
;;; Commentary:
;;; Code:


(use-package tuareg
  :ensure t
  :defer t)


(use-package merlin
  :ensure t
  :defer t
  :after tuareg
  :init
  ;; (setq-default merlin-command "~/.opam/default/bin/ocamlmerlin")
  :config
  (use-package merlin-iedit
    :disabled
    :ensure t
    :defer t)
  (use-package merlin-company
    :ensure t
    :defer t)
  :hook
  (tuareg-mode . merlin-mode)
  (caml-mode . merlin-mode))


(use-package opam-switch-mode
  :ensure t
  :defer t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))


(provide 'init-ocaml)
;;; init-ocaml.el ends here
