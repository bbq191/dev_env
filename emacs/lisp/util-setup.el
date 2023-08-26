;; init-base.el --- Util configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;;; Code:

;; Diminish
(use-package diminish   :ensure t)
(elpaca-wait)

;; Move Up/Down
(use-package move-dup   :ensure t
  :bind (([M-S-up] . move-dup-move-lines-up)
         ([M-S-down] . move-dup-move-lines-down)
         ("C-c d" . 'move-dup-duplicate-down)
         ("C-c u" . 'move-dup-duplicate-up)))
(elpaca-wait)
;; No Littering - Help keeping ~/.config/emacs clean
;; If you would like to use base directories different from what no-littering uses by default, then you have to set the respective variables before loading the feature.
;; (use-package no-littering)
;; (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
;; (setq no-littering-var-directory (expand-file-name "data/" user-emacs-directory))

;; recentf setting
(use-package recentf
  :elpaca nil
    
  :ensure t
  :init (recentf-mode)
  :config (setq recentf-max-saved-items 300)
  :hook (recentf-exclude . (recentf-expand-file-name no-littering-var-directory))
          (recentf-exclude . (recentf-expand-file-name no-littering-etc-directory)))
(elpaca-wait)

;; Fix Exec Path for Mac
(use-package exec-path-from-shell   :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))
(elpaca-wait)

;; TODO: need to move
(use-package general   :ensure t)
(elpaca-wait)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile is a project interaction library for Emacs
(use-package projectile
   
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config (dolist (dir '("bazel-bin"
                         "bazel-out"
                         "bazel-testlogs"))
            (add-to-list 'projectile-globally-ignored-directories dir))
  :custom
  (projectile-use-git-grep t)
  (projectile-indexing-method 'alien)
  (projectile-kill-buffers-filter 'kill-only-files)
  ;; Ignore uninteresting files. It has no effect when using alien mode.
  (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,package-user-dir)))
(elpaca-wait)

;; Icons Completion
;; Note: All-the-icons-completion depends on an already installed all-the-icons.
(use-package all-the-icons   :ensure t
  :if (display-graphic-p))
(elpaca-wait)

(use-package all-the-icons-completion   :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(elpaca-wait)

;; Page Break Lines
(use-package page-break-lines   :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :custom (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family)))
(elpaca-wait)

;; ;; Which Key
;; (use-package which-key
;;   :init (which-key-mode 1)
;;   :diminish
;;   :config (setq which-key-side-window-location 'bottom
;;                 which-key-sort-order #'which-key-key-order-alpha
;;                 which-key-allow-imprecise-window-fit nil
;;                 which-key-sort-uppercase-first nil
;;                 which-key-add-column-padding 1
;;                 which-key-max-display-columns nil
;;                 which-key-min-display-lines 6
;;                 which-key-side-window-slot -10
;;                 which-key-side-window-max-height 0.25
;;                 which-key-idle-delay 0.8
;;                 which-key-max-description-length 25
;;                 which-key-allow-imprecise-window-fit nil
;;                 which-key-separator " → " ))

(provide 'util-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-util.el ends here
