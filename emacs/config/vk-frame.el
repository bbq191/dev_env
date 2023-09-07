;; vk-frame.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.728)
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

;; Fonts
(set-face-attribute 'default nil :family '("Cascadia Code") :height 130)
(set-fontset-font t 'symbol (font-spec :family "Symbols Nerd Font") nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
(set-fontset-font t 'han (font-spec :family "Source Han Sans SC"))

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
