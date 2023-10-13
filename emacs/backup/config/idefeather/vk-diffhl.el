;; vk-diffhl.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; looks fairly visually appealing.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
     (delete . " ")
     (change . " ")
     (unknown . "?")
     (ignored . "i"))))

;; The code-review package allows for integration with pull request comments and such.
(use-package emojify)

(provide 'vk-diffhl)
;;; vk-diffhl.el ends here