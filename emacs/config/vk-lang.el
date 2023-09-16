;; vk-lang.el --- vk-lang configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Languages
(use-package lua-mode)

(use-package yaml-mode
  :hook (yaml-mode . prog-mode)
  :config
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'"))

(use-package toml-mode
  :hook (toml-mode . prog-mode))

(provide 'vk-lang)

;;; vk-lang.el ends here
