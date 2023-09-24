;; vk-dabbrev.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dabbrev
  :bind* (("C-/" . #'dabbrev-completion))
  :custom
  (dabbrev-check-all-buffers t)
  (dabbrev-case-replace nil))

(provide 'vk-dabbrev)
;;; vk-dabbrev.el ends here
