;; vk-dap.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Rust和Go这种命令式语言,使用逐步调试器更有必要。
(use-package dap-mode
  :disabled
  :bind
  (:map dap-mode-map
        ("C-c b b" . dap-breakpoint-toggle)
        ("C-c b r" . dap-debug-restart)
        ("C-c b l" . dap-debug-last)
        ("C-c b d" . dap-debug))
  :init
  (require 'dap-go)
  ;; NB: dap-go-setup appears to be broken, so you have to download the
  ;; extension from GH, rename its file extension
  ;; unzip it, and copy it into the config so that the following path lines up
  (setq dap-go-debug-program '("node" "/Users/afu/.config/emacs/.extension/vscode/golang.go/extension/dist/debugAdapter.js"))
  (defun pt/turn-on-debugger ()
    (interactive)
    (dap-mode)
    (dap-auto-configure-mode)
    (dap-ui-mode)
    (dap-ui-controls-mode)))

(provide 'vk-dap)
;;; vk-dap.el ends here
