;;; package -- init-ui.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;; theme: modus
(use-package emacs
  :ensure nil
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-syntax '(faint yellow-comments green-strings alt-syntax))
  (modus-themes-syntax '(yellow-comments green-strings alt-syntax))
  (modus-themes-mixed-fonts nil)
  (modus-themes-links '(neutral-underline faint bold italic background))
  (modus-themes-prompts '(bold intense))
  ;; (modus-themes-mode-line '(accented 3d borderless (padding 2) (height 1.0)))
  (modus-themes-tab-accented t)
  (modus-themes-completions '((matches . (extrabold background intense))
                              (selection . (italic semibold accented intense))
                              (popup . (accented))))
  (modus-themes-fringe 'intense)
  (modus-themes-lang-checkers '(straight-underline text-also background faint))
  (modus-themes-hl-line '(accented intense))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-intense-mouseovers t)
  (modus-themes-markup '(intense background))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-region '(accented))
  (modus-themes-diffs 'desaturated)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (background overline rainbow 2.0))
                           (1 . (background overline rainbow 1.8))
                           (2 . (background overline rainbow 1.6))
                           (3 . (background overline rainbow 1.4))
                           (4 . (background overline rainbow 1.2))
                           (t . (overline semibold))))
  :bind
  ("<f12>" . #'modus-themes-toggle)
  :hook
  (emacs-startup . (lambda () (load-theme 'modus-operandi-tinted))))



;;; rainbow delimiters
(use-package rainbow-delimiters
  :defer
  :hook
  (after-init . rainbow-delimiters-mode))



;;; dashboard
(use-package dashboard
  :pin melpa
  :custom
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))



(provide 'init-ui)
;;; init-ui.el ends here
