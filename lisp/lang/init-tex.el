;;; package --- tex mode for editing tex documentations
;;; Commentary:
;;; Code:

(use-package tex
  :ensure auctex
	:defer t
  :after cdlatex
  :custom
  (preview-image-type 'dvipng)
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil)
	(push (list 'output-pdf "Zathura") TeX-view-program-selection)
	:bind ((:map TeX-mode-map
	             ("C-c 4" . my/latex-dollars)
               ("C-c 5" . my/latex-ddollars))
         (:map LaTeX-mode-map
	             ("C-c 4" . my/latex-dollars)))
  :hook (((TeX-mode LaTeX-mode)
          . (lambda () (add-to-list 'completion-at-point-functions
                               #'cdlatex-capf)))
         (LaTeX-mode . turn-on-cdlatex)))

;;; fast latex insertion
(use-package cdlatex
  :defer t
  :after cl-lib
  :config
  ;; implement a `cdlatex' capf
  (defvar cdlatex-command-alist-comb-keys
    (cl-map 'list #'car cdlatex-command-alist-comb)
    "A list of possibles for cdlatex.")
  (defun cdlatex-capf ()
    "Native CAPF version of company-cdlatex-backend."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (list beg end
                cdlatex-command-alist-comb-keys
                :exclusive 'no
                :annotation-function (lambda (_) " cd LaTeX →")
                :exit-function
                (lambda (string status)
                  (cdlatex-tab))))))))

;;; custom features

(define-skeleton my/latex-dollars
	"Insert Dollars."
	"" ?\$ _ ?\$)

(define-skeleton my/latex-ddollars
	"Insert Double Dollars."
	"" "$$" _ "$$")

(define-skeleton my/latex-bs-parens
  "Insert \\(  \\)."
  "" "\\( " _ " \\)")

(define-skeleton my/latex-bs-brackets
  "Insert \\[ _  \\]."
  "\\[" \n _ \n "\\]")

(defun my/text-scale-adjust-latex-previews (&optional arg)
  "Adjust the size of latex preview fragments when changing text scale."
  (pcase major-mode
    ('LaTeX-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook
          #'my/text-scale-adjust-latex-previews)

(provide 'init-tex)
;;; init-tex.el ends here
