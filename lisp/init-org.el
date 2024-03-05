;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:


(use-package org
	:ensure nil
	;; :init
	;; (setq org-hide-emphasis-markers t)
	:config
	(setq org-format-latex-options
				(plist-put org-format-latex-options :scale 2.0))
	:bind
	(:map org-mode-map
				("C-c C-4" . (lambda () (interactive)
											 (skeleton-insert '(nil "\\( " _ " \\)"))))
				("C-c C-5" . (lambda () (interactive)
											 (skeleton-insert '(nil "\\[" \n _ \n "\\]"))))))


(use-package ox-latex
	:ensure nil
	:defer t
	:config
	(add-to-list 'org-latex-classes
							 '("ctexart" "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
	 (haskell . t)))


(use-package zotxt
	:defer t
	:hook
	(org-mode . org-zotxt-mode)
	:bind
	(:map org-mode-map
				("C-c \" \"" . (lambda () (interactive)
												 (org-zotxt-insert-reference-link '(4))))))


(use-package org-ref
	:defer t)


;; org-mode for blog: hugo
(use-package ox-hugo
	:after ox
	:init
	;; change this to your Hugo root
	(setq-default org-hugo-base-dir "~/Projects/HugoBlog/"))


;; org-mode for notes: roam
(use-package org-roam
	:defer t
	:hook
	(after-init . org-roam-db-autosync-mode)
	:init
	(unless (file-exists-p "~/org-roam")
		(make-directory "~/org-roam"))
	(setq org-roam-directory (file-truename "~/org-roam")))

(use-package org-roam-ui
	:defer t
	:after org-roam)


 (provide 'init-org)
;;; init-org.el ends here
