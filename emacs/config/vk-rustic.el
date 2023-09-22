;; vk-rustic.el --- vk-rustic configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; Rust
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (defun rk/rustic-mode-hook ()
    "Save on format."
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(use-package rust-playground)

(provide 'vk-rustic)

;;; vk-rustic.el ends here

