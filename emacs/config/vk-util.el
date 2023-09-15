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
  (persp-mode))

;; iseatch optmize
(use-package anzu
  :init (global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  
  (general-define-key
   [remap query-replace-regexp] 'anzu-query-replace-regexp
   [remap query-replace] 'anzu-query-replace))
(with-eval-after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))
  
  (provide 'vk-util)

;;; vk-util.el ends here
