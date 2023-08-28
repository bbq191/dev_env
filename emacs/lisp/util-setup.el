;; init-base.el --- Util configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;;; Code:

;; Diminish
(use-package diminish :ensure t)
(elpaca-wait)

;; Move Up/Down
(use-package move-dup
  :ensure t
  :bind (([M-S-up] . move-dup-move-lines-up)
         ([M-S-down] . move-dup-move-lines-down)
         ("C-c d" . 'move-dup-duplicate-down)
         ("C-c u" . 'move-dup-duplicate-up)))

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


;; Fix Exec Path for Mac
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

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


;; Icons Completion
;; Note: All-the-icons-completion depends on an already installed all-the-icons.
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))


;; Page Break Lines
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :custom (set-fontset-font "fontset-default"
                            (cons page-break-lines-char page-break-lines-char)
                            (face-attribute 'default :family)))


;; Quickly switch windows
(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face
                                      :foreground unspecified
                                      :bold t

                                      :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))
      (user-error "`toggle-window-split' only supports two windows")))

  ;; Bind hydra to dispatch list
  (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-util.el ends here
