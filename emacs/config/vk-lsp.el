;; vk-lsp.el --- vk-lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-M-." . consult-eglot-symbols))))
(provide 'vk-lsp)
;;; vk-lsp.el ends here
