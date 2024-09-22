;;; package --- early init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; ui settings
(add-to-list 'default-frame-alist '(tool-bar-mode . nil))
(add-to-list 'default-frame-alist '(menu-bar-mode . nil))
(add-to-list 'default-frame-alist '(scroll-bar-mode . nil))
(add-to-list 'default-frame-alist '(alpha-background . 95))

;; font
(add-to-list 'default-frame-alist '(font . "JuliaMono Nerd Font-16"))

;; full screen
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
