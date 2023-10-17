;;; package --- early init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set gc
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; ui settings
;(push '(scroll-bar-mode . nil) default-frame-alist)
;(push '(tool-bar-mode . nil) default-frame-alist)
;(push '(menu-bar-mode . nil) default-frame-alist)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(load-theme 'leuven)

(provide 'early-init-file)
;;; early-init.el ends here
