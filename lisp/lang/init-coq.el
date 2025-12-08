;;; package --- init-coq.el   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; coq
(use-package proof-general
  :pin melpa
  :after (company-coq
          opam-switch-mode)
  :init
  (setq proof-electric-terminator-enable nil)
  (setq proof-toolbar-enable t)

  :bind
  ( :map coq-mode-map
    ("C-M-<up>"    . proof-undo-last-successful-command)
    ("C-M-<down>"  . proof-assert-next-command-interactive)
    ("C-M-<right>" . proof-goto-point)
    ("M-;"   . my/coq-comment)
    ("C-M-;" . my/coq-doc)
    ("RET" . (lambda ()
               (interactive)
               (let ((indent-line-function #'indent-relative-first-indent-point))
                 (message "custom newline indent")
                 (newline-and-indent)))))
  ( :repeat-map coq-repeat-mode-map
    ("n"   . proof-assert-next-command-interactive)
    ("p"   . proof-undo-last-successful-command)
    ("u"   . proof-undo-last-successful-command)
    ("C-n" . proof-assert-next-command-interactive)
    ("C-p" . proof-undo-last-successful-command)
    ("C-u" . proof-undo-last-successful-command)
    :exit
    ("g"   . keyboard-quit)))



(use-package company-coq
  :defer t
  :init
  (setq company-coq-live-on-the-edge t)
  (setq company-coq-disabled-features '(prettify-symbols))
  :bind (:map company-coq-map
              ("RET" . nil))
  :hook
  (coq-mode
   . (lambda ()
       (interactive)
       (setq indent-line-function #'tab-to-tab-stop)
       (company-coq-mode t)
       (corfu-mode -1)
       (setq-local company-box-doc-enable nil)
       ))
  (coq-mode . window-tool-bar-mode))


(define-skeleton my/coq-comment
  "Insert a comment pattern."
  "" "(* " _ " *)")

(define-skeleton my/coq-doc
  "Insert a comment pattern."
  "" "(** " _ " *)")

(provide 'init-coq)
;;; init-coq.el ends here
