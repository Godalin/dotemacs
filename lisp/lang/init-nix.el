;;; package --- init-nix.el   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack
             nix-shell-configure
             nix-shell-build))

(use-package nix-repl
  :defer t
  :ensure nix-mode
  :commands (nix-repl))

(use-package nixos-options
  :defer t)

(use-package company-nixos-options
  :defer t
  :config
  (add-to-list 'company-backends
               'company-nixos-options))

(use-package nix-sandbox)

;;; direnv &/ envrc
(use-package envrc
  :disabled
  :defer t
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(use-package direnv
  :defer t
  :hook (after-init . direnv-mode))

(provide 'init-nix)
;;; init-nix.el ends here
