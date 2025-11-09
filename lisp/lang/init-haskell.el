;;; package --- init haskell settings
;;; Commentary:
;;; Code:



;;; haskell-mode
(use-package haskell-mode
  :defer t
  :after (eglot company)
  :bind
  (:map
   haskell-mode-map
   ;; editing
   ("C-c C-d" . 'haskell-mode-jump-to-def)
   ("C-c C-m" . 'haskell-navigate-imports)
   ;; info
   ("C-c C-t" . 'haskell-process-do-type)
   ("C-c C-i" . 'haskell-process-do-info)
   ("C-c C-c" . 'haskell-compile)
   ;; for repl buffers
   ("C-c C-l" . 'haskell-process-load-file)
   ("C-c C-`" . 'haskell-interactive-bring)
   ("C-c C-k" . 'haskell-interactive-mode-clear)
   ;; for cabal commands
   ("C-c c"   . 'haskell-process-cabal)
   ("C-c C-v" . 'haskell-cabal-visit-file)
   :map
   haskell-cabal-mode-map
   ("C-c C-c" . 'haskell-compile)
   ("C-c C-`" . 'haskell-interactive-bring)
   ("C-c C-k" . 'haskell-interactive-mode-clear)
   ;; for cabal commands
   ("C-c c"   . 'haskell-process-cabal))
  :hook
  ((haskell-mode . eglot-ensure)
   (haskell-mode . haskell-auto-insert-module-template)
   (haskell-mode . haskell-decl-scan-mode)
   (haskell-mode . (lambda ()
                     (set (make-local-variable 'company-backends)
                          (append '((company-capf company-dabbrev-code))
                                  company-backends))))))

(use-package haskell-interactive-mode
	:ensure nil
	:after haskell-mode
	:hook (haskell-mode . interactive-haskell-mode))

(use-package alex-mode
	:ensure nil
	:load-path "lisp/lang/modes"
	:defer t
	:mode ("\\.x\\'" . alex-mode))

(provide 'init-haskell)
;;; init-haskell.el ends here
