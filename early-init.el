;;; package --- early init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; ;; ui settings
;; (add-to-list 'default-frame-alist '(tool-bar-mode . nil))
;; (add-to-list 'default-frame-alist '(menu-bar-mode . nil))
;; (add-to-list 'default-frame-alist '(scroll-bar-mode . nil))
;; ;; (add-to-list 'default-frame-alist '(alpha-background . 0))

;; ;; font
;; (add-to-list 'default-frame-alist '(font . "JuliaMono Nerd Font-16"))

;; ;; full screen
;; ;(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))




;;; New version


;; setup gc
;; this will be recovered in `init.el'
;; (setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-threshold most-positive-fixnum)


;; UI 设置
(setf (alist-get 'tool-bar-mode default-frame-alist) nil
      (alist-get 'menu-bar-mode default-frame-alist) nil
      (alist-get 'scroll-bar-mode default-frame-alist) nil
      (alist-get 'alpha-background default-frame-alist) 70)

;; 字体
(add-to-list 'default-frame-alist '(font . "JuliaMono Nerd Font-16"))

;; 最大化窗口
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)



;;; early-init.el ends here.
