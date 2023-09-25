;; vk-magit.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Magit is one of the top three reasons anyone should use Emacs.
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-diff-refine-hunk t)
  (magit-repository-directories '(("~/Workspace" . 1)))
  (magit-list-refs-sortby "-creatordate")
  :config
  (defun pt/commit-hook () (set-fill-column 80))
  (add-hook 'git-commit-setup-hook #'pt/commit-hook)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; Magit also allows integration with GitHub and other such forges
(use-package forge :after magit)

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


(provide 'vk-magit)
;;; vk-magit.el ends here
