;; vk-theme.el --- init configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(global-hl-line-mode t)

(use-package smart-mode-line-powerline-theme)

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

;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :ensure t
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

(use-package smart-mode-line
  :init (progn (setq sml/theme 'respectful
                     sml/shorten-directory t
                     sml/no-confirm-load-theme t
                     sml/name-width 32
                     sml/shorten-modes t)
               (sml/setup)))


(provide 'vk-theme)

;;; vk-theme.el ends here
