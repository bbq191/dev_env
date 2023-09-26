;; vk-mics.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-this
  :bind ("C-c G" . #'google-this))



;; I use direnv to manage per-project environment variables.
(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))



(provide 'vk-mics)
;;; vk-mics.el ends here
