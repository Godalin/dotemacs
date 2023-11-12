;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:


(use-package org
	:config
	(setq org-format-latex-options
			(plist-put org-format-latex-options :scale 2.0)))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
	 (haskell . t)))


(use-package ox-hugo
	:pin melpa
	:after ox
	:init
	(setq-default org-hugo-base-dir "~/Projects/HugoBlog/"))

(provide 'init-org)
;;; init-org.el ends here
