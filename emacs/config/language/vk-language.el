;; vk-language.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Note that for this to work you have to add the tree-sitter ELPA server.
(shut-up
  (use-package tree-sitter
    :config (global-tree-sitter-mode))
  (use-package tree-sitter-langs))

;; Handier some language no needs more config
(use-package yaml-mode)
(use-package toml-mode)
(use-package protobuf-mode)

(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package web-mode
  :custom (web-mode-markup-indent-offset 2)
  :mode ("\\.html.erb$" . web-mode)
  :mode ("\\.art$" . web-mode))

(use-package typo)

(use-package makefile-executor
  :bind ("C-c M" . makefile-executor-execute-project-target))

(provide 'vk-language)
;;; vk-language.el ends here
