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

;; GC optimization
(use-package gcmh
  :diminish gcmh-mode
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;; Call undotree
(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))


(provide 'vk-util)

;;; vk-util.el ends here
