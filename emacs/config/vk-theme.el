;; vk-theme.el --- init configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(global-hl-line-mode t)

(use-package doom-themes
  :defer nil
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Sets the default theme to load!!
  (load-theme 'doom-opera t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;Display input key and command
(use-package keycast
  :commands (+toggle-keycast)
  :config
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON")))
  :hook (after-init . +toggle-keycast))

;; Mode-line
(use-package doom-modeline
  :defer nil
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
        doom-modeline-height 30      ;; sets modeline height
        doom-modeline-bar-width 10    ;; sets right bar width
        doom-modeline-persp-name t
        doom-modeline-minor-modes t))

(provide 'vk-theme)

;;; vk-theme.el ends here
