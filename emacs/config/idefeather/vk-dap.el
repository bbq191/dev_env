;; vk-dap.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Rust和Go这种命令式语言,使用逐步调试器更有必要。
(use-package dap-mode
  :after dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-codelldb)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)

  (setq dap-auto-configure-features '(sessions locals controls tooltip))

  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "Rust::Debug"
	     :miDebuggerPath "~/.local/share/cargo/bin/rust-lldb"
         ;; uncomment if lldb-mi is not in PATH
         ;; :lldbmipath "path/to/lldb-mi"
         )))
(provide 'vk-dap)
;;; vk-dap.el ends here
