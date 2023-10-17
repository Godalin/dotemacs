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

(provide 'init-tex)
;;; init-tex.el ends here
