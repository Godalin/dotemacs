;;; package -- init-ui.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Color Theme: Modus-Themes
(use-package modus-themes
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
  (modus-themes-completions '((matches   . (extrabold background intense))
                              (selection . (italic semibold accented intense))
                              (popup     . (accented))))
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
  :bind ("<f12>" . modus-themes-toggle))

;;; make use of the header line
(setq-default
 header-line-format
 (list "%e"
       'header-line-indent
       'mode-line-front-space
       ;; '(:propertize
       ;;   (""
       ;;    mode-line-mule-info
       ;;    mode-line-client
       ;;    mode-line-modified
       ;;    mode-line-remote
       ;;    mode-line-window-dedicated)
       ;;   display
       ;;   (min-width (6.0)))
       '(:eval (abbreviate-file-name default-directory))
       " : "
       "%b"
       mode-line-end-spaces))

;;; rainbow delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

;;; dashboard
(use-package dashboard
  :pin melpa
  :after nerd-icons
  :custom
  ;; initial buffer
  (initial-buffer-choice
   (lambda () (get-buffer-create dashboard-buffer-name)))
  ;; centering
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  ;; contents
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (registers . 5)))
  (dashboard-navigation-cycle t)
  ;; icons
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  ;; icon height
  ;; (dashboard-icon-file-height 1.75)
  ;; (dashboard-icon-file-v-adjust -0.125)
  ;; (dashboard-heading-icon-height 1.75)
  ;; (dashboard-heading-icon-v-adjust -0.125)
  :config
  (dashboard-setup-startup-hook))

;;; nerd-icons for ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-ui)
;;; init-ui.el ends here
