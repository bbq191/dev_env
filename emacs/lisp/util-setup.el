;; init-base.el --- Util configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;;; Code:

;; Diminish
;;  (use-package diminish)

;; Move Up/Down
  (use-package move-dup)
  (global-set-key [M-S-up] 'move-dup-move-lines-up)
  (global-set-key [M-S-down] 'move-dup-move-lines-down)

  (global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
  (global-set-key (kbd "C-c u") 'move-dup-duplicate-up)

;; No Littering - Help keeping ~/.config/emacs clean
;; If you would like to use base directories different from what no-littering uses by default, then you have to set the respective variables before loading the feature.
(setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
(use-package no-littering)

;; recentf setting
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 300)
(add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))

;; Fix Exec Path for Mac
  (use-package exec-path-from-shell
    :ensure t
    :when (eq system-type 'darwin)
    :hook (after-init . exec-path-from-shell-initialize))

;; Marginalia - Marginalia is painless to set up
  (use-package marginalia
   ; :general
   ; (:keymaps 'minibuffer-local-map
   ;           "M-A" 'marginalia-cycle)
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'right)
    :init
    (marginalia-mode))

;; Completion - Auto completed for corfu config.
  (use-package company)
  (use-package yasnippet
    :ensure
    :config
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'text-mode-hook 'yas-minor-mode))

;; Icons Completion
;; Note: All-the-icons-completion depends on an already installed all-the-icons.
  (use-package all-the-icons
    :ensure t
    :if (display-graphic-p))

  (use-package all-the-icons-completion
    :after (marginalia all-the-icons)
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :init (all-the-icons-completion-mode))

;; Page Break Lines
  (use-package page-break-lines
    :hook (after-init . global-page-break-lines-mode)
    :init (page-break-lines-mode))

(provide 'util-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-util.el ends here
