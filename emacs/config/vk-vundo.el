;; vk-vundo.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vundo
  :diminish
  :bind* (("C-c _" . vundo))
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(provide 'vk-vundo)
;;; vk-vundo.el ends here
