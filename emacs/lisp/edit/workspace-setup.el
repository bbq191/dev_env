;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Workspace configurations.
;;

;;; Code:

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           (not vk-icon))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-nerd-icons
    :demand t
    :when (vk/icons-displayable-p)
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-tab-bar
    :demand t
    :config (treemacs-set-scope-type 'Tabs)))

(provide 'workspace-setup)

;;; workspace-setup.el ends here
