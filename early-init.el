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

;; UI Settings
(setf (alist-get 'tool-bar-mode default-frame-alist) nil
      (alist-get 'menu-bar-mode default-frame-alist) nil
      (alist-get 'scroll-bar-mode default-frame-alist) nil
      (alist-get 'alpha-background default-frame-alist) 0.3
      )

;; fonts
(add-to-list 'default-frame-alist '(font . "JuliaMono Nerd Font-16"))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))

(when (eq system-type 'darwin)
  ;; (add-to-list 'default-frame-alist '(alpha . (80 70)))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;; early-init.el ends here.
