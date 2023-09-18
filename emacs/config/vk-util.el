;; vk-util.el --- vk-util configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Sudo edit especilly in edite remote file
(use-package sudo-edit)

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (emacs-lisp-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package perspective
  :init
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :bind
  ("C-x C-b" . persp-list-buffers))

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

;; Call undotree
(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(provide 'vk-util)

;;; vk-util.el ends here
