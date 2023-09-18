;; vk-theme.el --- init configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Sets the default theme to load!!
  (load-theme 'doom-zenburn t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-total-line-number nil)
  (setq doom-modeline-display-default-persp-name nil)
  (setq doom-modeline-unicode-fallback nil))


(provide 'vk-theme)

;;; vk-theme.el ends here
