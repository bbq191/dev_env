;; vk-dabbrev.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; completion extension
(use-package dabbrev
  :bind* (("C-/" . #'dabbrev-completion))
  :custom
  (dabbrev-check-all-buffers t)
  (dabbrev-case-replace nil))
(add-hook 'prog-mode-hook #'abbrev-mode)
(setq abbrev-suggest t)

(use-package fancy-compilation :config (fancy-compilation-mode))

(provide 'vk-dabbrev)
;;; vk-dabbrev.el ends here
