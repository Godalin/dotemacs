;;; package -- init-ui.el
;;; Commentary:
;;; Code:


;; theme: modus
(use-package emacs
	:ensure nil
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
	(modus-themes-italic-constructs t)
	(modus-themes-syntax '(faint yellow-comments green-strings alt-syntax))
	(modus-themes-mixed-fonts nil)
	(modus-themes-links '(neutral-underline faint bold italic background))
	(modus-themes-prompts '(background bold intense))
  (modus-themes-mode-line '(accented 3d borderless (padding 2) (height 1.0)))
	(modus-themes-tab-accented t)
	(modus-themes-completions '((matches . (extrabold background intense))
															(selection . (italic semibold accented intense))
															(popup . (accented))))
	(modus-themes-fringe 'intense)
	(modus-themes-lang-checkers '(straight-underline text-also background faint))
  (modus-themes-hl-line '(accented intense underline))
	(modus-themes-subtle-line-numbers t)
	(modus-themes-intense-mouseovers t)
	(modus-themes-markup '(intense background))
	(modus-themes-paren-match '(bold intense underline))
  (modus-themes-region '(accented))
	(modus-themes-diffs 'desaturated)
	(modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (background overline rainbow 2.0))
													 (1 . (background overline rainbow 1.8))
													 (2 . (background overline rainbow 1.6))
													 (3 . (background overline rainbow 1.4))
													 (4 . (background overline rainbow 1.2))
													 (t . (overline semibold))))
  :config
  (load-theme 'modus-vivendi)

	;; get the default mode-line color
	(setq-default default-mode-line-color
								(cons (face-background 'mode-line)
											(face-foreground 'mode-line)))
  :bind
  ("<f12>" . 'modus-themes-toggle))


(defun my/set-modeline ()
	"Change the color of the modeline."
	(let ((color
				 (cond ((minibufferp) '("#f5426f" . "#66ccff"))
							 ((and (fboundp 'evil-normal-state-p)
										 (evil-normal-state-p)) '("#66ccff" . "#000000"))
							 ((and (fboundp 'evil-insert-state-p)
										 (evil-insert-state-p)) '("#9cf542" . "#000000"))
							 ((and (fboundp 'evil-visual-state-p)
										 (evil-visual-state-p)) '("#473ce6" . "#ffffff"))
							 ((and (fboundp 'evil-replace-state-p)
										 (evil-replace-state-p)) '("#8e8f94" . "#ffffff"))
							 ((and (fboundp 'evil-operator-state-p)
										 (evil-operator-state-p)) '("#8e8f94" . "#ffffff"))
							 (t default-mode-line-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))


;; dashboard
(use-package dashboard
	:disabled
  :pin melpa
	:init
	(setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))


(provide 'init-ui)
;;; init-ui.el ends here
