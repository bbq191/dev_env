;; vk-mics.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-this)

;; I use direnv to manage per-project environment variables.
(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

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

(provide 'vk-mics)
;;; vk-mics.el ends here
