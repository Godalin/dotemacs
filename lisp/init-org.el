;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :defer t
  ;; :init
  ;; (setq org-hide-emphasis-markers t)

  :custom
  ;; `org-directory' is "~/org"
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-return-follows-link nil)

  :config
  (use-package org-tempo
    :ensure nil
    :after org)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  ;; enabled source languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (haskell . t)
     (scheme . t)
     ;; rackets
     (racket . t)
     ;; (scribble . t)
     ))
  ;; add structure templates
  (dolist (temp '(("rs" . "src racket :noweb-ref ? :eval no")
                  ("rt" . "src racket")
                  ("rq" . "src shell :results output html :exports results")))
    (add-to-list 'org-structure-template-alist temp))

  :bind (:map org-mode-map
              ("C-c C-4" . (lambda () (interactive)
                             (skeleton-insert
                              '(nil "\\( " _ " \\)"))))
              ("C-c C-5" . (lambda () (interactive)
                             (skeleton-insert
                              '(nil "\\[" \n _ \n "\\]"))))
              ("C-c b" . org-switchb))

  :hook (org-mode . (lambda ()
                      (modify-syntax-entry ?< "." (syntax-table))
                      (modify-syntax-entry ?> "." (syntax-table)))))

;;; add cn support for latex
(use-package ox-latex
  :ensure nil
  :defer t
  :after org
  :config
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;; use org with Zotero
(use-package zotxt
  :defer t
  :after org
  :bind (:map org-mode-map
              ("C-c \" \"" . (lambda () (interactive)
                               (org-zotxt-insert-reference-link '(4)))))
  :hook (org-mode . org-zotxt-mode))

;;; org-ref
(use-package org-ref
  :defer t
  :after org)

;;; org for blog: ox-hugo
(use-package ox-hugo
  :defer t
  :after org ox
  :init
  (let ((hugo-blog (file-truename "~/Projects/HugoBlog")))
    (when (and (file-exists-p hugo-blog)
               (file-directory-p hugo-blog))
      (setq-default org-hugo-base-dir
                    (file-truename hugo-blog)))))

;;; org-roam is fantastic
(use-package org-roam
  :defer t
  :after org
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :init
  (unless (file-exists-p "~/org-roam")
    (make-directory "~/org-roam"))
  :bind
  (:map org-mode-map
        ("C-c r i" . org-roam-node-insert)
        ("C-c r f" . org-roam-node-find)
        ("C-c r c" . org-roam-capture))
  :hook (after-init . org-roam-db-autosync-mode))

(use-package org-roam-ui
  :defer t
  :after org-roam)

(provide 'init-org)
;;; init-org.el ends here
