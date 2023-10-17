;;; package --- This is module manages the packages that are not builtin with GNU Emacs
;;; Commentary:
;;; Code:



;; use-package deal with package
(use-package package
  :config
  (setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			   ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  ;;; if not initialized then initialize it
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (package-refresh-contents)
  )



;; which key
(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode t)
  )



;; company completion
(use-package company
  :ensure t
  :defer t
  :config
  (setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3))
	company-tooltip-align-annotations t
	company-tooltip-margin 2)
  :hook
  (
   (after-init . global-company-mode)
   (haskell-mode . (lambda ()
		     (set (make-local-variable 'company-backends)
			  (append '((company-capf company-dabbrev-code))
				  company-backends))))
   )
  :bind
  (:map
   company-active-map
   ("RET" . company-complete-common)
   ([return] . company-complete-common)
   ("TAB" . company-complete-selection)
   ([tab] . company-complete-selection))
  )



;; format-all
(use-package format-all
  :ensure t
  :defer t)



;; ivy-counsel-swiper completion
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  )



;;; Programming Languages

;; julia mode
(use-package julia-mode
  :ensure t)

;; coq
(use-package proof-general
  :ensure t
  :defer t)

(use-package company-coq
  :ensure t
  :defer t
  :hook (coq-mode . company-coq-mode))

;; common lisp
(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))



(provide 'packages)
;;; packages.el ends here
