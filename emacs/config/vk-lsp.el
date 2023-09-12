;; vk-lsp.el --- vk-lsp configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; lsp-mode
(use-package lsp-mode
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred))
  :general (:keymaps 'lsp-mode-map
                     "SPC cf" #'lsp-format-region
                     "SPC cd" #'lsp-describe-thing-at-point
                     "SPC ce" #'lsp-execute-code-action
                     "SPC cr" #'lsp-rename)
  ;; :config
  ;; (with-no-warnings
  ;; (lsp-enable-which-key-integration t))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.9)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(provide 'vk-lsp)

;;; vk-lsp.el ends here
