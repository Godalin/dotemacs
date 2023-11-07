;;; package --- tex mode for editing tex documentations
;;; Commentary:
;;; Code:

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (push (list 'output-pdf "Zathura") TeX-view-program-selection)
  )

(use-package cdlatex
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . 'turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(provide 'init-tex)
;;; init-tex.el ends here
