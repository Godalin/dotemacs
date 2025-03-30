;;; package --- Packages that are not builtin with GNU Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;;; env
;; (use-package exec-path-from-shell
;;   :defer t
;;   ;; :config
;;   ;; (when (memq window-system '(mac ns x))
;;   ;;   (exec-path-from-shell-initialize))
;;   :hook
;;   (emacs-startup . exec-path-from-shell-initialize)
;;   )

;;; company
(use-package company
  :defer t
  :commands (company-abort
	           company-complele-selection)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-tooltip-align-annotations t)
  (company-tooltip-margin 2)
  :bind
  (:map
   company-active-map
   ("RET" . #'company-abort)
   ([return] . #'company-abort)
   ("TAB" . #'company-complete-selection)
   ([tab] . #'company-complete-selection))
  :hook
  (emacs-startup . global-company-mode))

(use-package company-box
  :defer t
  :after company
  :hook
  (company-mode . company-box-mode))

(use-package company-posframe
  :disabled
  :defer t
  :after company
  :hook
  (company-mode . company-posframe-mode))

;;; ivy-counsel-swiper completion
(use-package counsel
  :defer t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "[ %d/%d ]")
  :bind
  ([remap isearch-forward]  . #'swiper-isearch)
  ([remap isearch-backward] . #'swiper-isearch)
  ([remap execute-extended-command] . #'counsel-M-x)
  ([remap find-file] . #'counsel-find-file)
  ([remap yank-pop] . #'counsel-yank-pop)
  ([remap describe-function] . #'counsel-describe-function)
  ([remap describe-variable] . #'counsel-describe-variable)
  ([remap find-library] . #'counsel-find-library)
  ([remap info-lookup-symbol] . #'counsel-info-lookup-symbol)
  ([remap switch-to-buffer] . #'ivy-switch-buffer)
  ([remap recentf-open] . #'counsel-recentf)
  ([remap bookmark-jump] . #'counsel-bookmark)
  ("<f2> i" . #'info-lookup-symbol)
  ("<f2> u" . #'counsel-unicode-char)
  ("<f2> j" . #'counsel-set-variable)
  ("<f2> v" . #'ivy-push-view)
  ("<f2> s" . #'ivy-save-view)
  ("<f2> V" . #'ivy-pop-view)
  (:map
   ivy-minibuffer-map
   ("M-RET" . #'ivy-immediate-done))
  :hook
  (emacs-startup . ivy-mode))

(use-package marginalia
  :defer t
  :bind
  ;; (:map
  ;;  minibuffer-local-map
  ;;  ("M-A" . marginalia-cycle))
  :hook
  (emacs-startup . marginalia-mode))

;;; avy is awesome
(use-package avy
  :defer t
  :config
  (setq avy-timeout-seconds 0.8)
  :bind
  ("C-:" . #'avy-goto-char)
  ("C-;" . #'avy-goto-char-timer)
  ("C-'" . #'avy-goto-char-2)
  ("M-g f" . #'avy-goto-line)
  ("M-g w" . #'avy-goto-word-1)
  ("M-g e" . #'avy-goto-word-0))

;;; ace-window
(use-package ace-window
  :defer t
  :bind
  ("M-o" . #'ace-window))

;;; magit
(use-package magit
  :defer t
  :commands (magit)
  :custom
  (magit-view-git-manual-method 'woman))

;;; highlight the line diff
(use-package diff-hl
  :defer t
  :hook
  (emacs-startup . global-diff-hl-mode))

;;; yet an other snippet
(use-package yasnippet
  :defer t
  :after yasnippet-snippets
  :hook
  (emacs-startup . yas-global-mode))

(use-package yasnippet-snippets
  :defer t)

;;; keycast
(use-package keycast
  :defer t
  :commands (keycast-tab-bar-mode)
  :custom-face
  (keycast-key ((t (:height 100))))
  (keycast-command ((t (:height 100))))
  :hook
  (tab-bar-mode . keycast-tab-bar-mode))

;;; multiple cursors
(use-package multiple-cursors
  :defer t
  :bind
  ("C-S-c C-S-c" . #'mc/edit-lines)
  ("C-S-c C-S-a" . #'mc/edit-beginnings-of-lines)
  ("C-S-c C-S-e" . #'mc/edit-beginnings-of-lines)
  ("C-S-c C-<" . #'mc/mark-all-like-this)
  ("C-<" . #'mc/mark-previous-like-this)
  ("C->" . #'mc/mark-next-like-this)
  ("C-S-c C-S-<right>" . #'mc/mark-next-like-this-word)
  ("C-S-c C-S-<left>" . #'mc/mark-previous-like-this-word))

;;; iedit
(use-package iedit
  :disabled
  :defer t)

;;; sexy mode line
(use-package smart-mode-line
  :defer t
  :init
  (setq sml/no-confirm-load-theme t)
  :hook
  (emacs-startup . sml/setup))

;;; mini mode line
(use-package mini-modeline
  :disabled
  :after smart-mode-line
  :config
  (mini-modeline-mode))

;;; mini frame
(use-package mini-frame
  :disabled
  :defer t
  :custom (mini-frame-show-parameters
           '((top . 0.3)
             (width . 0.7)
             (left . 0.5)))
  :hook
  (emacs-startup . mini-frame-mode))

;;; sr-speedbar
(use-package sr-speedbar
  :defer t)

;;; eshell
(use-package eshell-toggle
  :defer t
  :custom (eshell-toggle-find-project-root-package 'project)
  :commands (eshell-toggle))

;;; diren
(use-package envrc
  :hook
  (emacs-startup . envrc-global-mode))

;;; Rime
(use-package rime
  :defer t
  :commands (rime-send-keybinding)
  :custom
  (default-input-method "rime")
  (rime-librime-root "/opt/homebrew/")
  (rime-emacs-module-header-root
   "/Applications/Emacs.app/Contents/Resources/include")
  (rime-show-candidate 'posframe)
  :bind
  (:map
   rime-mode-map
   ("C-`" . #'rime-send-keybinding)
   ("<f4>" . #'rime-send-keybinding)))

;;; hydra
(use-package hydra
  :defer t)

;;; tree sitter
(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  :hook
  (emacs-startup . global-treesit-auto-mode))

(provide 'init-packages)
;;; init-packages.el ends here
