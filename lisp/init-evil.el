;;; package -- Emacs Modal Editing: with Vim  -*- lexical-binding: t; -*-

;;; Commentary:

;;; The most convenient way to edit code is to take advantage of both
;;; Emacs and Vim bindings:
;;;
;;; normal movement:
;;;   emacs
;;;
;;; text objects:
;;;   vim
;;; C-g exit evil mode

;;; Code:

;;; use `general' to configure `evil' with `use-package'
(use-package general)

;;; `evil' is nice but only in normal mode
(use-package evil
  :defer t
  :after general
  :custom
  (evil-toggle-key "C-<escape>")
  (evil-want-C-i-jump t)
  (evil-want-C-u-delete t)
  (evil-default-state 'normal)
  (evil-auto-indent t)
  (evil-shift-width 2)
  (evil-shift-round t)
  (evil-indent-convert-tabs t)
  (evil-move-beyond-eol nil)

  ;; set cursor
  (evil-emacs-state-cursor '(bar "#66CCFF"))
  (evil-normal-state-cursor '(box "brown"))

  ;; set modeline notation colors
  :custom-face
  (my/evil-normal-face
   ((t (:foreground "white" :background "orange" :weight bold))))
  (my/evil-insert-face
   ((t (:foreground "white" :background "purple" :weight bold))))
  (my/evil-visual-face
   ((t (:foreground "white" :background "green" :weight bold))))
  (my/evil-replace-face
   ((t (:foreground "white" :background "red" :weight bold))))
  (my/evil-motion-face
   ((t (:foreground "white" :background "cyan" :weight bold))))
  (my/evil-emacs-face
   ((t (:foreground "white" :background "blue" :weight bold))))

  :config
  ;; override mode line tag behavior
  (defun my/evil-generate-mode-line-tag (&optional state)
    "Generate the evil mode-line tag for STATE."
    (let ((tag (evil-state-property state :tag t)))
      (when (functionp tag)
        (setq tag (funcall tag)))
      ;; prepare mode-line: add tooltip
      (if (stringp tag)
          (propertize
           tag
           'face (cond
		              ((string= "normal" state)
		               'my/evil-normal-face)
		              ((string= "insert" state)
		               'my/evil-insert-face)
		              ((string= "visual" state)
		               'my/evil-visual-face)
		              ((string= "emacs" state)
		               'my/evil-emacs-face))
           'help-echo (evil-state-property state :name)
           'mouse-face 'mode-line-highlight)
        tag)))

  (advice-add 'evil-generate-mode-line-tag :override
              #'my/evil-generate-mode-line-tag)

  :bind ((:map text-mode-map
               ("<escape>" . evil-force-normal-state))
         (:map prog-mode-map
               ("<escape>" . evil-force-normal-state)))
  :general
  (:states '(normal visual)
           "C-e" 'end-of-visual-line
           "TAB" 'indent-for-tab-command)
  :hook ((evil-insert-state-entry . evil-emacs-state)
         ;; (activate-mark . evil-emacs-state)
         (evil-normal-state-entry
          . (lambda () (setq display-line-numbers-type 'relative)))
         (evil-normal-state-exit
          . (lambda () (setq display-line-numbers-type t)))
         ))

;; evil-surround
(use-package evil-surround
  :defer t
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

;; evil with tex objects
(use-package evil-textobj-syntax
  :defer t
  :after evil evil-surround)

(provide 'init-evil)
;;; init-evil.el ends here.
