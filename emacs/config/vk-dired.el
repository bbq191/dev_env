;; vk-dired.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode)
  (setq dired-recursive-deletes 'top)
  (use-package dired-x))


(use-package dired-preview
  :after dired
  :hook (dired-mode . dired-preview-mode))
 :general (:keymaps dired-mode-map 
              "h" #'dired-up-directory
              "l" #'dired-open-file
           :keymaps dired-preview-mode-map
           "j" #'dired-preview-next-file
           "k" #'dired-preview-prev-file))


(provide 'vk-dired)

;;; vk-dired.el ends here
