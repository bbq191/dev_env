;; init-base.el --- Util configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;;; Code:

;; Diminish
(use-package diminish :ensure t)
(elpaca-wait)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :bind (("M-g M-l" . avy-goto-line)
         ("M-g M-j" . avy-goto-char-timer))
  :custom
  (avy-background t)
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (avy-styles-alist '((avy-isearch . pre))))

;; Move Up/Down
(use-package move-dup
  :ensure t
  :bind (([M-S-up] . move-dup-move-lines-up)
         ([M-S-down] . move-dup-move-lines-down)
         ("C-c d" . 'move-dup-duplicate-down)
         ("C-c u" . 'move-dup-duplicate-up)))

;; recentf setting
(use-package recentf
  :elpaca nil
  :ensure t
  :init (recentf-mode)
  :config (setq recentf-max-saved-items 300)
  :hook (recentf-exclude . (recentf-expand-file-name no-littering-var-directory))
  (recentf-exclude . (recentf-expand-file-name no-littering-etc-directory)))

;; Fix Exec Path for Mac
(when (or (and (display-graphic-p) is-macsys) (daemonp))
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

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

;; undotree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

;; mwim stands for Move Where I Mean.
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))


;; The blazing grep tool
;; Press C-c s to search
(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings))

;; Page Break Lines
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :custom (set-fontset-font "fontset-default"
                            (cons page-break-lines-char page-break-lines-char)
                            (face-attribute 'default :family)))


;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))
;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :ensure t
  :hook (after-init . smart-region-on))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; util-setup.el ends here
