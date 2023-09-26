;; vk-treesitter.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Note that for this to work you have to add the tree-sitter ELPA server.
(shut-up
  (use-package tree-sitter
    :config (global-tree-sitter-mode))

  (use-package tree-sitter-langs))

(provide 'vk-treesitter)
;;; vk-treesitter.el ends here