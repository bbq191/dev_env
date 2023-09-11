;; vk-font.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Inhibit resizing font

(require 'cl-lib) ;; cl-loop dependence

;; Fonts
(defun vk/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Source Code Pro")
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 130)))
  
  ;; Specify font for all unicode characters
  (cl-loop for font in '("Symbols Nerd Font" "Symbols Nerd Font Mono" "Symbol")
           return (set-fontset-font t 'symbol (font-spec :family font)))

  ;; Emoji
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Emoji")
           return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Source Han Sans CN" "PingFang SC" "Microsoft Yahei" "STFangsong")
           return (progn
                    (setq face-font-rescale-alist `((,font . 0.95)))
                    (set-fontset-font t 'han (font-spec :family font))))

  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(vk/setup-fonts)
(add-hook 'window-setup-hook #'vk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'vk/setup-fonts)

(provide 'vk-font)

;;; vk-font.el ends here
