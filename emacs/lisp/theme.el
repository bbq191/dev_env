(set-face-attribute 'default nil
    :font "Cascadia Code"
    :height 130
    :weight 'regular)
(set-face-attribute 'variable-pitch nil
    :font "Symbols Nerd Font"
    :height 130
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

(use-package autothemer
   :ensure t
   :elpaca (:host github
             :repo "jasonm23/autothemer"))
(add-to-list 'custom-theme-load-path "~/.config/emacs/theme/")
(load-theme 'rose-pine t)
