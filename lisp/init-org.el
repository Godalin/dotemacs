;;; package --- initialization of Org-mode and others
;;; Commentary:
;;; Code:


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (haskell . t)))

(setq-default org-format-latex-options
      (plist-put org-format-latex-options :scale 3.0))


(use-package ox-hugo
  :disabled
  :ensure t)


(provide 'init-org)
;;; init-org.el ends here
