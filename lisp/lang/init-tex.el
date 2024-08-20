;;; package --- tex mode for editing tex documentations
;;; Commentary:
;;; Code:


(use-package tex
  :ensure auctex
	:defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
	(push (list 'output-pdf "Zathura") TeX-view-program-selection)
	:bind
	(:map
	 TeX-mode-map
	 ("C-c 4" . 'dollars)
	 ("C-c C-4" . 'double-dollars)))


(use-package cdlatex
  :defer t
  :hook
  ;; (LaTeX-mode . 'turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(define-skeleton dollars
	"Insert Dollars."
	"dollars"
	?\$ _ ?\$)

(define-skeleton double-dollars
	"Insert Dollars."
	"double dollars"
	"$$" _ "$$")


(provide 'init-tex)
;;; init-tex.el ends here
