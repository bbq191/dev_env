;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Better defaults.
;;
;;; Code:

;; rustic = basic rust-mode + additions
(use-package rustic
  :ensure t
;;   :bind (:map rustic-mode-map
;;          ("M-j" . lsp-ui-imenu)
;;          ("M-?" . lsp-find-references)
;;          ("C-c C-c l" . flycheck-list-errors)
;;          ("C-c C-c a" . lsp-execute-code-action)
;;          ("C-c C-c r" . lsp-rename)
;;          ("C-c C-c q" . lsp-workspace-restart)
;;          ("C-c C-c Q" . lsp-workspace-shutdown)
;;          ("C-c C-c s" . lsp-rust-analyzer-status)
;;          ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
;;          ("C-c C-c d" . dap-hydra)
;;          ("C-c C-c h" . lsp-ui-doc-glance))
  :config 
  ;; (push 'rustic-clippy flycheck-checkers)
  (setq rustic-format-on-save t)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  :custom (rustic-lsp-setup-p nil)
  (rustic-lsp-client nil)
  (lsp-rust-server 'rustic-lsp-server)
  ;; (rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))
  (rustic-flycheck-clippy-params "--message-format=json -Zunstable-options")
  (rustic-rustfmt-args "+nightly")
  (rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))

  (add-hook 'rustic-mode-hook 'kv/rustic-mode-hook)

  (defun kv/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t)))

;; Rust Playground Create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure t)

;; Cargo.toml and other config files
(use-package toml-mode)


(provide 'rust-setup)
;;; rust-setup.el ends here
