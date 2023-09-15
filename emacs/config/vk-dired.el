;; vk-dired.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

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
