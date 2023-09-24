;; vk-base.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
              indent-tabs-mode nil     ; Permanently indent with spaces, never with TABs
              truncate-lines t
              display-line-numbers-width 3
              indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?\u254e)

(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      blink-cursor-mode nil             ; No eyes distraction
      column-number-mode t
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t

      ;; Suppress GUI features
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Some pretty config from prucell
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(delete-selection-mode 1)    ;; You can select text and delete it by typing.

;; Display wrape line
(global-display-fill-column-indicator-mode 1)
(global-visual-line-mode 1)

;; Show line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Eemacs true transparent
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Fonts
(defun vk/setup-fonts ()
  "Setup fonts."
  ;; Set default font
  (set-face-attribute 'default nil :font "Iosevka Fixed" :height 150)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Fixed" :height 150)

  ;; Specify font for all unicode characters
            (set-fontset-font t 'symbol (font-spec :font "Symbols Nerd Font Mono"))

  ;; Emoji
            (set-fontset-font t 'emoji (font-spec :font "Apple Color Emoji") nil 'prepend)

  ;; Specify font for Chinese characters
 ;;(progn (setq face-font-rescale-alist `((,font . 0.95))))
                    (set-fontset-font t 'han (font-spec :font "Source Han Sans CN"))

  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(vk/setup-fonts)
(add-hook 'window-setup-hook #'vk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'vk/setup-fonts)

(provide 'vk-base)
;;; vk-base.el ends here
