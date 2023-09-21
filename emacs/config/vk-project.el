;; vk-project.el --- vk-project configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package projectile
  :config
  (projectile-mode 1))

;; file tree viewer
(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

;; workspaces
;; (use-package perspective
;;   :custom
;;   ;; NOTE! I have also set 'SCP =' to open the perspective menu.
;;   ;; I'm only setting the additional binding because setting it
;;   ;; helps suppress an annoying warning message.
;;   (persp-mode-prefix-key (kbd "C-c M-p"))
;;   :init
;;   (persp-mode)
;;   :config
;;   ;; Sets a file to write to when we save states
;;   (setq persp-state-default-file (no-littering-expand-var-file-name "sessions")))

;; ;; This will group buffers by persp-name in ibuffer.
;; (add-hook 'ibuffer-hook
;;           (lambda ()
;;             (persp-ibuffer-set-filter-groups)
;;             (unless (eq ibuffer-sorting-mode 'alphabetic)
;;               (ibuffer-do-sort-by-alphabetic))))

;; ;; Automatically save perspective states to file when Emacs exits.
;; (add-hook 'kill-emacs-hook #'persp-state-save)
(provide 'vk-project)

;;; vk-project.el ends here
