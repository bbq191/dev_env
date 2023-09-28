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
(use-package forge 
  :after magit)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(provide 'vk-magit)
;;; vk-magit.el ends here
