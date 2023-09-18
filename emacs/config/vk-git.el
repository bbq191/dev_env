;; vk-git.el --- Git is awesome -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-paint-whitespace nil)
  (magit-ediff-dwim-show-on-hunks t))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :after vc
  :init (global-diff-hl-mode)
  :hook ((dired-mode         . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  ;; (unless (display-graphic-p)
  (diff-hl-margin-mode))

;; Visual diff interface
(use-package ediff
  ;; Restore window config after quitting ediff
  :hook ((ediff-before-setup . ediff-save-window-conf)
         (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Setup gitignore mode
(use-package conf-mode
  :mode (("\\.gitignore\\'"     . conf-unix-mode)
         ("\\.gitconfig\\'"     . conf-unix-mode)
         ("\\.gitattributes\\'" . conf-unix-mode)))

(use-package git-timemachine
  :after git-timemachine
  :general
  (:keymap git-timemachine-mode-map
           "C-j" 'git-timemachine-show-previous-revision
           "C-k" 'git-timemachine-show-next-revision))

(provide 'vk-git)

;;; vk-git.el ends here
