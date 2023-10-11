;; vk-search.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; sometimes deadgrep is more useful.
(use-package deadgrep
  :ensure-system-package rg
  :bind (("C-c H" . #'deadgrep)))

;; I remember the days before Emacs had real regular expressions. Nowadays, we
;; have them, but the find-and-replace UI is bad. visual-regexp fixes this.
(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

;; Add anzu
(use-package anzu
  :diminish t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(provide 'vk-search)
;;; vk-search.el ends here
