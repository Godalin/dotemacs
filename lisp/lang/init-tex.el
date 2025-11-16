;;; package --- tex mode for editing tex documentations
;;; Commentary:
;;; Code:

;;; use `auctex' for `tex'
(use-package tex
  :ensure auctex
	:defer t
  :after (cdlatex preview-dvisvgm)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-image-type 'dvisvgm)
  (preview-auto-cache-preamble t)
  (preview-scale-function 2.0)
  (preview-default-preamble
   '("\\usepackage[displaymath,sections,graphics,floats,textmath]{preview}"
     "\\PreviewEnvironment[{[]}]{tikzpicture}"
     "\\usepackage{nccmath}"
     "\\everydisplay{\\fleqn}"))

  :config
  (use-package preview-dvisvgm
    :config
    (advice-add 'preview-dvisvgm-start :override
                #'my/preview-dvisvgm-start))
  
  (add-to-list 'TeX-view-program-list
               '("Sioyek" "sioyek %o" "sioyek"))
	(add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Sioyek"))

	:bind ((:map TeX-mode-map
	             ("C-c 4" . my/latex-dollars)
               ("C-c 5" . my/latex-ddollars)))
  :hook ((LaTeX-mode
          . (lambda () (add-to-list 'completion-at-point-functions
                               #'cdlatex-capf)))
         (LaTeX-mode . turn-on-cdlatex)))

;;; fast latex insertion
(use-package cdlatex
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
  "Adjust the size of latex preview fragments when changing text scale.
The `ARG' parameter is for hooks."
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
  "Helper function for text-scale latex previews.
The `OV' parameter is the overlay."
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook
          #'my/text-scale-adjust-latex-previews)

(defun my/preview-dvisvgm-start ()
  "Start a DviSvgm process.  
Redefine the default scale factor."
  (when (> preview-dvisvgm-debug 0)
    (message "Running `preview-dvisvgm-start'."))
  (let* (;; (file preview-gs-file)
         tempdir
         (scale (* (/ (preview-hook-enquiry preview-scale)
                      (preview-get-magnification))
		               (with-current-buffer TeX-command-buffer
		                 (if text-scale-mode
		                     (expt text-scale-mode-step
                               text-scale-mode-amount)
                       ;; use the scale function provided by preview
                       2.0
                       ))))
         (scale-str  (format " --scale=%g " scale))
         (command (with-current-buffer TeX-command-buffer
                    (prog1
                        (concat (TeX-command-expand
				                         (if (or TeX-PDF-mode
                                         preview-parsed-pdfoutput)
				                             preview-dvisvgm-pdf-command
				                           preview-dvisvgm-command))
                                " " scale-str)
                      (setq tempdir TeX-active-tempdir))))
         (name "Preview-DviSVGM"))
    (setq TeX-active-tempdir tempdir)
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
          (lambda (_process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
        (let ((process (start-process name (current-buffer) TeX-shell
                                      TeX-shell-command-option
                                      command)))
          (if TeX-after-start-process-function
              (funcall TeX-after-start-process-function process))
          (TeX-command-mode-line process)
          (set-process-filter process #'TeX-command-filter)
          (set-process-sentinel process #'TeX-command-sentinel)
          (set-marker (process-mark process) (point-max))
          (push process compilation-in-progress)
          (sit-for 0)
          process)
      (setq mode-line-process ": run")
      (force-mode-line-update)
      (call-process TeX-shell nil (current-buffer) nil
                    TeX-shell-command-option
                    command))))

(provide 'init-tex)
;;; init-tex.el ends here
