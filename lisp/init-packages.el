;;; package --- Packages that are not builtin with GNU Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-keymaps)

;;; company is only used by coq
;;; use corfu now in new versions
(use-package company
  :defer t
  :diminish company-mode
  :commands company-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-tooltip-align-annotations t)
  (company-tooltip-margin 2)
  :bind ( :map company-active-map
          ("RET"    . company-abort)
          ([return] . company-abort)
          ("TAB"    . company-complete-selection)
          ([tab]    . company-complete-selection)))

(use-package company-box
  :after company
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))



;;; modern completion package
(use-package corfu
  :defer 2
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'show)
  (corfu-popupinfo-delay 0.1)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :bind ( :map corfu-map
          ("RET" . nil)))

;; ;;; flexible combinable completion backends
;; (use-package cape
;;   :bind ("C-c p" . cape-prefix-map)
;;   ;; :init
;;   ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   ;; (add-hook 'completion-at-point-functions #'cape-file)
;;   ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   ;; (add-hook 'completion-at-point-functions #'cape-history)
;;   )

;;; icons for `corfu'
(use-package kind-icon
  :defer 1
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-icon-margin-formatter))

;;; yet another snippet template system
(use-package yasnippet
  :defer 10
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets)
  :bind ( :map yas-minor-mode-map
          ("TAB"   . nil)
          ("<tab>" . nil)))

;; ;;; `DEPRECATED:::' ivy-counsel-swiper
;; ;;; use vertico now in new versions
;; (use-package counsel
;;   :disabled
;;   :defer t
;;   :diminish ivy-mode
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "[ %d/%d ]")
;;   :bind (([remap isearch-forward]    . swiper-isearch)
;;          ([remap isearch-backward]   . swiper-isearch)
;;          ([remap execute-extended-command] . counsel-M-x)
;;          ([remap find-file]          . counsel-find-file)
;;          ([remap yank-pop]           . counsel-yank-pop)
;;          ([remap describe-function]  . counsel-describe-function)
;;          ([remap describe-variable]  . counsel-describe-variable)
;;          ([remap find-library]       . counsel-find-library)
;;          ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
;;          ([remap switch-to-buffer]   . ivy-switch-buffer)
;;          ([remap recentf-open]       . counsel-recentf)
;;          ([remap bookmark-jump]      . counsel-bookmark)
;;          ("<f2> i" . info-lookup-symbol)
;;          ("<f2> u" . counsel-unicode-char)
;;          ("<f2> j" . counsel-set-variable)
;;          ("<f2> v" . ivy-push-view)
;;          ("<f2> s" . ivy-save-view)
;;          ("<f2> V" . ivy-pop-view)
;;          (:map ivy-minibuffer-map
;;                ("M-RET" . #'ivy-immediate-done)))
;;   :hook (emacs-startup . ivy-mode))

;;; vertico for new versions
(use-package vertico
  :defer 1
  :custom
  ;; (vertico-scroll-margin 10)
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode))

;;; ivy-style completion
(use-package orderless
  :defer 1
  :custom
  ;; eamcs completion settings
  ;; this part can be sent to `init.el'
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-styles
   '(orderless basic substring partial-completion flex))
  (completion-category-overrides '((file (styles partial-completion))))
  
  ;; Emacs 31: partial-completion behaves like substring
  ;; (completion-pcm-leading-wildcard t)
  )

;;; add annotations in minibuffer
(use-package marginalia
  :defer 2
  :init
  (marginalia-mode)

  :config
  (defun my/marginalia-annotate-command (func cand)
    "Annotate command CAND with additional information.
Add new information to `marginalia-annotate-symbol' in the advice style."
    (when-let ((sym (intern-soft cand)))
      (let* ((mode (if (boundp sym)
                       sym
                     (lookup-minor-mode-from-indicator cand)))
             (status (if mode
                         (if (and (boundp mode) (symbol-value mode))
                             (propertize " On" 'face 'marginalia-on)
                           (propertize " Off" 'face 'marginalia-off))
                       ""))
             (globalp (if mode
                          (if (local-variable-p mode)
                              (propertize " Lcl" 'face '(:foreground "cyan"))
                            (propertize " Glb" 'face '(:foreground "cyan")))
                        "")))
        (concat globalp status (funcall func cand)))))

  (advice-add #'marginalia-annotate-command :around
              #'my/marginalia-annotate-command)

  :bind ( :map minibuffer-local-map
          ("M-a" . marginalia-cycle)))

;;; add some icons
(use-package nerd-icons-completion
  :defer 10
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;;; consult
(use-package consult)

;;; embark is a fantastic menu package
(use-package embark
  :after vertico
  :init
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (setq embark-prompter 'embark-completing-read-prompter)
  (setq embark-indicators
        '(embark-minimal-indicator ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  :bind (("C-z k" . embark-act)
         ("C-z K" . embark-dwim)))

(use-package embark-consult
  :defer t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; avy is awesome
(use-package avy
  :custom
  (avy-timeout-seconds 0.8)
  :bind (("C-:"   . avy-goto-char)
         ("C-;"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

;;; ace-window
(use-package ace-window
  :defer 10
  :config
  (ace-window-posframe-mode)
  :bind ("M-o" . ace-window))

;;; better dired-mode
(use-package dirvish
  :defer 10
  :custom
  (dirvish-attributes
   (append '(vc-state subtree-state nerd-icons collapse)
           '(git-msg file-modes file-time file-size)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left
     (sort file-time " " file-size symlink)
     :right
     (omit yank index)))
  :config
  (dirvish-override-dired-mode))

;;; icons
(use-package nerd-icons)

;;; magit
(use-package magit
  :commands magit
  :custom
  (magit-view-git-manual-method 'woman))

;;; highlight the line diff
(use-package diff-hl
  ;; :defer 10
  :custom
  (diff-hl-fallback-to-margin t)
  ;; :config
  ;; (global-diff-hl-mode)
  :hook ((dired-mode . diff-hl-dired-mode)
         ((text-mode prog-mode) . diff-hl-margin-mode)
         ((text-mode prog-mode) . diff-hl-show-hunk-mouse-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;; keycast
(use-package keycast
  :custom-face
  (keycast-key     ((t (:height 100))))
  (keycast-command ((t (:height 100))))
  :hook (tab-bar-mode . keycast-tab-bar-mode))

;;; multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-e" . mc/edit-beginnings-of-lines)
         ("C-S-c C-<"   . mc/mark-all-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C->"         . mc/mark-next-like-this)
         ("C-S-c C-S-<right>" . mc/mark-next-like-this-word)
         ("C-S-c C-S-<left>"  . mc/mark-previous-like-this-word)))

;;; helpful
(use-package helpful
  :custom
  (helpful-switch-buffer-function #'my/helpful-switch-to-buffer)
  (helpful-max-buffers 5)

  :config
  (defun my/helpful-switch-to-buffer (buffer-or-name)
    "Show `BUFFER-OR-NAME' in an existing helpful window if possible.
Otherwise, pop to the buffer as usual."
    (let* ((buf (get-buffer buffer-or-name))
           (win (get-window-with-predicate
                 (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (eq major-mode 'helpful-mode))))))
      (if (and win buf)
          ;; Reuse the existing helpful window
          (progn
            (set-window-buffer win buf)
            (select-window win))
        ;; Fall back to creating new
        (pop-to-buffer buf))))

  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-key]      . helpful-key)
         ("C-c d" . helpful-at-point)))

;;; sr-speedbar
(use-package sr-speedbar
  :commands sr-speedbar-toggle)

;;; Rime
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "/opt/homebrew/")
  (rime-emacs-module-header-root
   (expand-file-name "include" user-emacs-directory))
  (rime-show-candidate 'posframe)
  :bind ( :map rime-mode-map
          ("C-`"  . rime-send-keybinding)
          ("<f4>" . rime-send-keybinding)))



(provide 'init-packages)
;;; init-packages.el ends here
