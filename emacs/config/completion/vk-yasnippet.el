;; vk-yasnippet.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

;; Load doom-snippet for code completion
(use-package doom-snippets
  :load-path "/Users/afu/Workspace/snippets"
  :after yasnippet)

(provide 'vk-yasnippet)

;;; vk-yasnippet.el ends here
