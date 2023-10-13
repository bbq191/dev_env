;; vk-flycheck.el --*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-fringe))

;; extension
;; (use-package flycheck-inline
;;   :after flycheck-mode
;;   :hook (flycheck-mode-hook . flycheck-inline-mode))

;; (use-package flycheck-rust
;;   :after rust-mode
;;   :hook (flycheck-mode-hook . flycheck-rust-setup))

;; (provide 'vk-flycheck)
;;; vk-flycheck.el ends here
