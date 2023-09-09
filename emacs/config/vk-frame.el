;; vk-frame.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Inhibit resizing frame

(require 'cl-lib) ;; cl-loop dependence

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.9)
                            (width . 0.528)
                            (height . 0.9)
                            (fullscreen)))

;; Title
(setq frame-title-format '("Vinci & Kate's Gnu Emacs - %b")
      icon-title-format frame-title-format)

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      blink-cursor-mode nil             ; No eyes distraction
                                        ;desktop-save nil                  ; Close desktop save
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)


;; Display wrape line
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

;; Fonts
;; (font-spec :family font :otf '(latn nil (calt zero ss01) nil))))
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
                    (setq face-font-rescale-alist `((,font . 0.8)))
                    (set-fontset-font t 'han (font-spec :family font))))

  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(vk/setup-fonts)
(add-hook 'window-setup-hook #'vk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'vk/setup-fonts)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Some pretty config from prucell
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
(setq-default
 display-line-numbers-width 3
 indicate-buffer-boundaries 'left
 display-fill-column-indicator-character ?\u254e)

(provide 'vk-frame)

;;; vk-frame.el ends here
