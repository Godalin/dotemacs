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

(use-package evil
  :defer t
  :custom
  (evil-toggle-key nil)
  (evil-want-C-i-jump t)
  (evil-want-C-u-delete t)
  (evil-default-state 'normal)
  (evil-auto-indent t)
  (evil-shift-width 2)
  (evil-shift-round t)
  (evil-indent-convert-tabs t)
  (evil-move-beyond-eol nil)
  :bind ("<escape>" . evil-mode)
  :hook ((evil-emacs-state-entry . evil-exit)
         (evil-normal-state-entry
          . (lambda ()
              (setq display-line-numbers-type 'relative)
              ;; (display-line-numbers-mode)
              ))
         (evil-normal-state-exit
          . (lambda ()
              (setq display-line-numbers-type t)
              ;; (display-line-numbers-mode)
              )))
  :init
  (defun evil-exit (&rest _)
    "command to exit evil mode"
    (interactive)
    (message "Exit Vim Simulation (Evil)")
    (evil-mode -1))
  :config
  ;; exit evil
  (evil-define-key
    '(normal insert visual replace operator motion emacs)
    'global (kbd "C-<escape>") 'evil-emacs-state))

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
