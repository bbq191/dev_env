;; vk-dabbrev.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; completion extension
(use-package dabbrev
  :bind* (("C-/" . #'dabbrev-completion))
  :custom
  (dabbrev-check-all-buffers t)
  (dabbrev-case-replace nil))


;; TODO: I want to use the fancy-dabbrev package everywhere,
;; but it uses popup.el rather than read-completion, and
;; I don't like how quickly it operates on its inline suggestions
(add-hook 'prog-mode-hook #'abbrev-mode)
(setq abbrev-suggest t)

(provide 'vk-dabbrev)
;;; vk-dabbrev.el ends here
