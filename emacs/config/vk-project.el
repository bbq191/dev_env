;; vk-project.el --- vk-project configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package projectile
  :config
  (projectile-mode 1))

;; workspaces
(use-package perspective
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode)
  :config
  ;; Sets a file to write to when we save states
  (setq persp-state-default-file (no-littering-expand-var-file-name "sessions")))
;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

;; This is hook perspctive with projectile
(use-package persp-projectile)


(provide 'vk-project)
;;; vk-project.el ends here
