;;; package --- Not only racket mode, but also other lisp.
;;; Commentary:
;;; Code:

;;; emacs lisp

;;; common lisp mode
(add-hook 'lisp-mode-hook
          (lambda ()
            (load (expand-file-name "~/.quicklisp/slime-helper.el"))
            (setq inferior-lisp-program "sbcl")))

;;; scheme
(use-package scheme-mode
  :ensure nil
  :defer t
  :custom (scheme-program-name "scheme"))

(use-package geiser-chez
  :defer t
  :custom (geiser-chez-binary "scheme"))

(use-package geiser-guile
  :defer t)

;;; racket
(use-package geiser-racket
  :defer t)

(use-package racket-mode
  :defer t)

(use-package ob-racket
  :after org
  :vc (ob-racket
       :url "https://github.com/hasu/emacs-ob-racket.git"
       :shell-command "raco make ob-racket-runtime*.rkt")
  :hook (ob-racket-pre-runtime-library-load
	       . ob-racket-raco-make-runtime-library))

(provide 'init-racket)
;;; init-racket.el ends here
