;; vk-mics.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-this
  :bind ("C-c G" . #'google-this))

;; By default, the list of recent files gets cluttered up with the contents of
;; downloaded packages.
(use-package recentf
  :pin gnu
  :after dash
  :init (pt/customize-tramp) ;; so that tramp urls work ok in recentf
  :custom
  ;; (recentf-exclude (-concat recentf-exclude '("\\elpa"
  ;;                                             "private/tmp" ; to avoid custom files
  ;;                                             "txt/roam"
  ;;                                             "type-break"
  ;;                                             )))
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 30)
  :config (recentf-mode))

;; I use direnv to manage per-project environment variables.
(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(defun my-default-window-setup ()
  "Called by 'emacs-startup-hook to set up my initial window configuration."
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (split-window-right)
  (other-window 1)
  (find-file "~/.config/emacs/init.el")
  (other-window 1))

(add-hook 'emacs-startup-hook #'my-default-window-setup)

(provide 'vk-mics)
;;; vk-mics.el ends here
