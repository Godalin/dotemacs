;;; package --- early init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; set gc
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))


;; ui settings
(add-to-list 'default-frame-alist '(tool-bar-mode . nil))
(add-to-list 'default-frame-alist '(menu-bar-mode . nil))
(add-to-list 'default-frame-alist '(scroll-bar-mode . nil))
(add-to-list 'default-frame-alist '(alpha-background . 85))


(provide 'early-init-file)
;;; early-init.el ends here
