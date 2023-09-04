;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq vk-theme 'pro)                           ; Color theme: system, default, pro, dark, light, warm, cold, day or night
(setq vk-http-proxy "127.0.0.1:6152")          ; HTTP/HTTPS proxy
(setq vk-socks-proxy "127.0.0.1:6153")         ; SOCKS proxy
(setq vk-server t)                             ; Enable `server-mode' or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-cn, bfsu, netease, sjtu, tencent, tuna or ustc
(setq vk-icon t)                               ; Display icons or not: t or nil
(setq vk-restore-frame-geometry t)             ; Restore the frame's geometry at startup: t or nil
(setq vk-completion-style 'childframe)         ; Completion display style: minibuffer or childframe
(setq vk-lsp 'lsp-mode)                        ; Set LSP client: lsp-mode, eglot or nil
(setq vk-lsp-format-on-save t)                 ; Auto format buffers on save: t or nil
(setq vk-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
(setq vk-tree-sitter t)                        ; Enable tree-sitter or not: t or nil. Only available in 29+.
(setq vk-prettify-symbols-alist t)             ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq vk-prettify-org-symbols-alist t)         ; Alist of symbol prettifications for `org-mode'

;; Enable proxy
(vk/proxy-http-enable)
(vk/proxy-socks-enable)

;; Display wrape line
(global-display-fill-column-indicator-mode 1)

;; Fonts -- todo  如何开启 otf 属性
;; (set-fontset-font t 'latin (font-spec :family "Cascadia Code" :otf '(latn nil (calt zero ss01) nil)))
(defun vk/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Source Code Pro")
             when (vk/font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (vk-mac 130)
                                                      (t 100))))
    ;; latin -- open otf
    (cl-loop for font in '("Cascadia Code")
             when (vk/font-installed-p font)
             return (set-fontset-font t 'latin (font-spec :family font :otf '(latn nil (calt zero ss01) nil))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Symbols Nerd Font" "Symbols Nerd Font Mono" "Symbol")
             when (vk/font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Apple Color Emoji" "Segoe UI Emoji")
             when (vk/font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("Source Han Sans CN" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (vk/font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.0)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))
(vk/setup-fonts)
(add-hook 'window-setup-hook #'vk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'vk/setup-fonts)
