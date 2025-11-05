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
  (evil-toggle-key "C-<escape>")
  (evil-want-C-i-jump t)
  (evil-want-C-u-delete t)
  (evil-default-state 'normal)
  (evil-auto-indent t)
  (evil-shift-width 2)
  (evil-shift-round t)
  (evil-indent-convert-tabs t)
  (evil-move-beyond-eol nil)
  :bind ("<escape>" . evil-mode)
  :hook ((evil-insert-state-entry . evil-emacs-state)
         (evil-normal-state-entry
          . (lambda () (setq display-line-numbers-type 'relative)))
         (evil-normal-state-exit
          . (lambda () (setq display-line-numbers-type t)))))

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
