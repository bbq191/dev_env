;; vk-flycheck.el --- vk-flycheck configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package flycheck
  :diminish t
  :init (global-flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

(use-package flycheck-rust
  :after rustic-mode
  :custom
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(provide 'vk-flycheck)

;;; vk-flycheck.el ends here
