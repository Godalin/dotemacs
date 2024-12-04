;;; package --- Packages that are not builtin with GNU Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package esup
  :defer t
  :pin melpa
  :commands (esup)
  :config
  (setq esup-depth 0))


(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; which key
(use-package which-key
  :defer t
  :commands (which-key-mode)
  :hook
  (after-init . which-key-mode))


;; keycast
(use-package keycast
  :defer t
  :commands (keycast-tab-bar-mode)
  :custom-face
  (keycast-key
   ((t (:height 120))))
  (keycast-command
   ((t (:height 120))))
  :hook
  (tab-bar-mode . keycast-tab-bar-mode))


;; company
(use-package company
  :defer t
  :commands (company-abort
	     company-complele-selection)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-tooltip-align-annotations t)
  (company-tooltip-margin 2)
  :hook
  (after-init . global-company-mode)
  (haskell-mode . (lambda ()
                    (set (make-local-variable 'company-backends)
                         (append '((company-capf company-dabbrev-code))
                                 company-backends))))
  :bind
  (:map
   company-active-map
   ("RET" . #'company-abort)
   ([return] . #'company-abort)
   ("TAB" . #'company-complete-selection)
   ([tab] . #'company-complete-selection)
   ;;("SPC" . #'company-complete-selection)
   ;;([space] . #'company-complete-selection)
   ))


;; format-all (not completed)
(use-package format-all
  :disabled
  :ensure t
  :defer t)


;; ivy-counsel-swiper completion
(use-package counsel
  :defer t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d")
  :hook
  (after-init . ivy-mode)
  :bind
  ("C-s" . 'swiper-isearch)
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("M-y" . 'counsel-yank-pop)
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> i" . 'counsel-info-lookup-symbol)
  ("<f2> u" . 'counsel-unicode-char)
  ("<f2> j" . 'counsel-set-variable)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-c v" . 'ivy-push-view)
  ("C-c V" . 'ivy-pop-view)
  (:map
   ivy-minibuffer-map
   ("M-<return>" . 'ivy-immediate-done))
  )


;;; version control

;; magit
(use-package magit
  :defer t
  :commands (magit)
  :custom
  (magit-view-git-manual-method 'woman))

;; direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))


(provide 'init-packages)
;;; init-packages.el ends here
