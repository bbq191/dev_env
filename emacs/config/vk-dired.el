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
  (use-package dired-x :elpaca nil))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-4-map "C-x C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(use-package dired-preview
  :after dired
  :hook (dired-mode . dired-preview-mode))
;; :config
;; (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
;; (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)) ; use dired-find-file instead if not using dired-open package
;; ;; (evil-define-key 'normal dired-preview-mode-map (kbd "j") 'dired-preview-next-file)
;; (evil-define-key 'normal dired-preview-mode-map (kbd "k") 'dired-preview-prev-file))


(provide 'vk-dired)

;;; vk-dired.el ends here
