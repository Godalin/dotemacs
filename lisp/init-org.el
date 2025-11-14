;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :defer t
  :after cdlatex
  :custom
  ;; `org-directory' is "~/org"
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-return-follows-link nil)
  (org-preview-latex-default-process 'dvisvgm)

  ;; markers
  (org-hide-emphasis-markers t)
  (org-link-descriptive t)
  (org-pretty-entities t)
  (org-hidden-keywords t)

  :config

  ;; load tempo templates
  (use-package org-tempo :ensure nil)

  ;; scale latex preview images
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))

  ;; enabled source languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (haskell . t)
     (scheme . t)
     (racket . t)))

  ;; add auto adjust after preview
  (advice-add #'org-latex-preview :after
              #'my/text-scale-adjust-latex-previews)

  ;; add structure templates
  (dolist (temp '(("rs" . "src racket :noweb-ref ? :eval no")
                  ("rt" . "src racket")))
    (add-to-list 'org-structure-template-alist temp))

  ;; remove <> as parentheses in org mode
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table)

  :bind (:map org-mode-map
              ("C-c C-4" . my/latex-bs-parens)
              ("C-c C-5" . my/latex-bs-brackets)
              ("C-c b"   . org-switchb))

  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . (lambda ()
                       (add-to-list 'completion-at-point-functions
                                    #'cdlatex-capf)))))

;;; automatic org-markup expansion in org
(use-package org-appear
  :defer t
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  :hook (org-mode . org-appear-mode))

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
              ("C-c \" \""
               . (lambda () (interactive)
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
  (let ((hugo-blog-dir (file-truename "~/Projects/HugoBlog")))
    (when (and (file-exists-p hugo-blog-dir)
               (file-directory-p hugo-blog-dir))
      (setq-default org-hugo-base-dir
                    hugo-blog-dir))))

;;; org-roam is fantastic
(use-package org-roam
  :defer t
  :after org
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :init
  (let ((org-roam-dir (file-truename "~/org-roam")))
    (unless (file-exists-p org-roam-dir)
      (make-directory org-roam-dir)))
  :bind (("C-z o r" . org-roam-capture)
         :map org-mode-map
         ("C-c r i" . org-roam-node-insert)
         ("C-c r f" . org-roam-node-find))
  :hook (after-init . org-roam-db-autosync-mode))

(use-package org-roam-ui
  :defer t
  :after org-roam)

(provide 'init-org)
;;; init-org.el ends here
