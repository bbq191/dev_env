;; vk-rustic.el --- vk-rustic configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Rust
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))

(use-package rust-playground)

(provide 'vk-rustic)

;;; vk-rustic.el ends here

