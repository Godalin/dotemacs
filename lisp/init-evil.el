;;; package -- quick edit in emacs with vim bindings  -*- lexical-binding: t; -*-

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
  :ensure t
	:bind
	("C-z C-z" . 'evil-mode)

  :hook
  (evil-emacs-state-entry . evil-exit)

	:init
  (setq evil-toggle-key "C-z C-z"
				evil-want-C-i-jump t
				evil-want-C-u-delete t

				evil-default-state 'normal

				evil-auto-indent t
				evil-shift-width 2
				evil-shift-round t
				evil-indent-convert-tabs t

				evil-move-beyond-eol nil
				)

  (defun evil-exit ()
    "command to exit evil mode"
    (interactive)
    (message "Exit Vim Simulation (Evil)")
    (evil-mode -1))


  :config
  ;; exit evil
  (evil-define-key '(normal insert visual replace operator motion emacs)
									 'global (kbd "C-g") 'evil-emacs-state)


  ;; evil-surround
  (use-package evil-surround
    :ensure t
    :hook
    (evil-mode . global-evil-surround-mode))

  ;; change mode-line color by evil state
	)


(provide 'init-evil)
;;; init-evil.el ends here.
