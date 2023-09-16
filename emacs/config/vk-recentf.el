;; vk-recentf.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; consult-recentf needs this on startup
;; Inhibit resizing recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq-default recentf-max-saved-items 1000
              recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(provide 'vk-recentf)

;;; vk-recentf.el ends here
