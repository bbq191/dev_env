;; vk-nerd-icons.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Font Nerd-Symbol-Mono is necessory
(use-package nerd-icons :demand t)

;; For dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; For completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; For treemacs
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; For ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'vk-nerd-icons)
;;; vk-nerd-icons.el ends here
