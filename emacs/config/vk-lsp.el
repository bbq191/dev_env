;; vk-lsp.el --- vk-lsp configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred))
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-region)
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action)
              ("C-c r" . lsp-rename))
  ;; :config
  ;; (with-no-warnings
  ;; (lsp-enable-which-key-integration t))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-links nil)                    ;; no clickable links
  (lsp-enable-folding t)                  ;; use `hideshow' instead
  (lsp-enable-snippet nil)                  ;; no snippets, it requires `yasnippet'
  (lsp-enable-file-watchers t)            ;; performance matters
  (lsp-enable-text-document-color t)      ;; as above
  (lsp-enable-symbol-highlighting t)      ;; as above
  (lsp-enable-on-type-formatting nil)       ;; as above
  (lsp-semantic-tokens-enable nil)          ;; optional
  (lsp-semantic-tokens-apply-modifiers nil) ;; don't override token faces
  (lsp-headerline-breadcrumb-enable t)    ;; keep headline clean
  (lsp-modeline-code-actions-enable t)    ;; keep modeline clean
  (lsp-modeline-diagnostics-enable t)     ;; as above
  (lsp-log-io nil)                          ;; debug only
  (lsp-auto-guess-root t)                   ;; Yes, I'm using projectile
  (lsp-completion-provider :none)           ;; don't add `company-capf' to `company-backends'
  (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)             ;; disable eldoc hover
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(use-package elisp-mode :elpaca nil)

(provide 'vk-lsp)

;;; vk-lsp.el ends here
