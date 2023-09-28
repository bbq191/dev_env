;; vk-recentf.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'vk-recentf)

;;; vk-recentf.el ends here
