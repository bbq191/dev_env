;; vk-language.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

;; Rust is one of my favorite languages in the world.
(use-package rust-mode
  :defer t
  :custom
  (rust-format-on-save t)
  (lsp-rust-server 'rust-analyzer))

(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c a t" . rustic-cargo-current-test)
              ("C-c m" . rustic-compile))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))


(use-package typescript-mode
  :custom (typescript-indent-level 2))
(setq-default js-indent-level 2)

(use-package js2-mode
  :hook (js2-mode . js2-imenu-extras-mode)
  :mode ("\\.js$" . js2-mode)
  :ensure t
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t))

(use-package xref-js2
  :ensure t
  :hook (js2-mode . pt/js-hook)
  :custom
  (xref-js2-search-program 'rg)
  :config
  (defun pt/js-hook ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(use-package yaml-mode :defer t)
(use-package toml-mode :defer t)
(use-package protobuf-mode :defer t)

(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package web-mode
  :custom (web-mode-markup-indent-offset 2)
  :mode ("\\.html.erb$" . web-mode)
  :mode ("\\.art$" . web-mode))

(use-package typo :defer t)

(use-package makefile-executor
  :bind ("C-c M" . makefile-executor-execute-project-target))


(provide 'vk-language)
;;; vk-language.el ends here
