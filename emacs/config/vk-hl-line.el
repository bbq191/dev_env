;; vk-hl-line.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(provide 'vk-hl-line)
;;; vk-hl-line.el ends here
