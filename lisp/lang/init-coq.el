;;; package --- init-coq.el   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; coq
(use-package proof-general
  :after (company-coq
          opam-switch-mode)

  :commands (proof-assert-next-command-interactive
	           proof-undo-last-successful-command
	           proof-goto-point)
  :init
  (setq proof-electric-terminator-enable nil)
  (setq proof-toolbar-enable t)

  :bind
  (:map
   coq-mode-map
   ("C-M-<up>" . #'proof-undo-last-successful-command)
   ("C-M-<down>" . #'proof-assert-next-command-interactive)
   ("C-M-<right>" . #'proof-goto-point)
   ("RET" . (lambda ()
              (interactive)
              (let ((indent-line-function #'indent-relative-first-indent-point))
                (message "custom newline indent")
                (newline-and-indent))
              ))
   )
  (:repeat-map
   coq-repeat-mode-map
   ("n" . #'proof-assert-next-command-interactive)
   ("p" . #'proof-undo-last-successful-command)
   ("u" . #'proof-undo-last-successful-command)
   ("C-n" . #'proof-assert-next-command-interactive)
   ("C-p" . #'proof-undo-last-successful-command)
   ("C-u" . #'proof-undo-last-successful-command)
   :exit
   ("g" . #'keyboard-quit)))



(use-package company-coq
  :defer t
  :init
  (setq company-coq-live-on-the-edge t)
  (setq company-coq-disabled-features '(prettify-symbols))
  :bind
  (:map
   company-coq-map
   ("RET" . nil))
  :hook
  (coq-mode . (lambda ()                                ;prepare for the coq mode
                (interactive)
		            (opam-switch-set-switch "coq-env") ;switch to a good coq environment
                (setq indent-line-function #'tab-to-tab-stop) ;adjust the indent function
                (company-coq-mode t)             ;enable company mode
                ))
  (coq-mode . window-tool-bar-mode))



(provide 'init-coq)
;;; init-coq.el ends here
