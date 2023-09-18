;; vk-dired.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode)
  (setq dired-recursive-deletes 'top)
  (require 'dired-x))


(use-package dired-preview
  :ensure t
  :after dired
  :hook (dired-mode . dired-preview-mode))


(provide 'vk-dired)

;;; vk-dired.el ends here
