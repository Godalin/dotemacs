;;; package --- init haskell settings
;;; Commentary:
;;; Code:

;; haskell mode
(use-package haskell-mode
  :ensure t
  :hook
  ((haskell-mode . haskell-auto-insert-module-template)))

(provide 'init-haskell)
;;; init-haskell.el ends here
