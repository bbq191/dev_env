;; vk-lsp.el --- vk-lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package eglot
  :diminish
  :hook (prog-mode . eglot-ensure)
  :config
  (use-package consult-eglot))

(provide 'vk-lsp)
;;; vk-lsp.el ends here
