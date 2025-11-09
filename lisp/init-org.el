;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :defer t
  ;; :init
  ;; (setq org-hide-emphasis-markers t)
  :custom
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-return-follows-link nil)

  :config
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
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

  :bind (:map org-mode-map
              ("C-c C-4" . (lambda () (interactive)
                             (skeleton-insert
                              '(nil "\\( " _ " \\)"))))
              ("C-c C-5" . (lambda () (interactive)
                             (skeleton-insert
                              '(nil "\\[" \n _ \n "\\]"))))
              ("C-c b" . org-switchb)))

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

(use-package zotxt
  :defer t
  :bind (:map org-mode-map
              ("C-c \" \"" . (lambda () (interactive)
                         (org-zotxt-insert-reference-link '(4)))))
  :hook (org-mode . org-zotxt-mode))

;;; org-ref
(use-package org-ref
  :defer t)

;;; org-mode for blog: hugo
(use-package ox-hugo
  :defer t
  :after ox
  :init
  (let ((hugo-blog "~/Projects/HugoBlog"))
    (when (and (file-exists-p hugo-blog)
               (file-directory-p hugo-blog))
      (setq-default org-hugo-base-dir
                    (file-truename hugo-blog)))))

;;; org-mode for notes: roam
(use-package org-roam
  :defer t
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
