;; vk-centered-window.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; given the considerable size of my screen.
(use-package centered-window
  :custom
  (cwm-centered-window-width 180))

;; Compilation buffers should wrap their lines.
(add-hook 'compilation-mode-hook 'visual-line-mode)

;; URLs should be highlighted and linkified.
(global-goto-address-mode)

(provide 'vk-centered-window)
;;; vk-centered-window.el ends here
