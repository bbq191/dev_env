;; vk-codereview.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emojify)
;; review
(use-package code-review
  :custom
  (forge-owned-accounts '(("patrickt" . nil)))
  (code-review-auth-login-marker 'forge)
  (code-review-fill-column 80)
  (code-review-new-buffer-window-strategy #'switch-to-buffer-other-window)
  :after (magit forge emojify)
  :bind (:map forge-pullreq-section-map (("RET" . #'forge-browse-dwim)
                                         ("C-c r" . #'code-review-forge-pr-at-point)))
  :bind (:map forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
  :bind (:map code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
                                    ("N" . #'code-review-comment-jump-next)
                                    ("P" . #'code-review-comment-jump-previous)
                                    ("C-c p" . #'code-review-comment-jump-previous))))

(provide 'vk-codereview)
;;; vk-codereview.el ends here
