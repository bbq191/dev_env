;; init-base.el --- GRAPHIC AND THEME configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; GRAPHIC AND THEME
;;
;;; Code:

;; Fonts
;; 设置自己喜欢的字体
(set-face-attribute 'default nil
                    :font "Cascadia Code"
                    :height 130
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Symbols Nerd Font"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "FiraCode Nerd Font"
                    :height 130
                    :weight 'regular)

(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Cascadia Code-13"))

;; 中文字体修正
(set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "Source Han Sans CN" :size 12) nil 'prepend)

;; Theme
;; Rose Pine - 个人最喜欢的 theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/etc/theme/")
(use-package autothemer :ensure t)
;;(load-theme 'rose-pine t)

;; doomemacs themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'theme-setup)

;;; init-theme.el ends here


