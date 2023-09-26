;; vk-vc.el --- vc is awesome -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; Git
(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package magit)

(provide 'vk-vc)
;;; vk-vc.el ends here
