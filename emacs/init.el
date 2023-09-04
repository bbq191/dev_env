;; init.el --- init configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  ;; 自定义函数，变量及改建
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "custom" dir)))
  ;; 可以作为基本的编辑器的好用的配置
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "edit" dir)))
  ;; 开发者基本配置
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "util" dir)))
  ;; 开发语言专项配置
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "program" dir))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-cl-loop-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))
;; Requisites;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'custom-setup)
(require 'func-setup)
;; Package management
(require 'package-setup)

;; Preferences;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'base-setup)
(require 'keybind-setup)
(require 'hydra-setup)
(require 'ui-setup)
(require 'edit-setup)
(require 'completion-setup)
(require 'corfu-setup)
(require 'bookmark-setup)
(require 'dired-setup)
(require 'highlight-setup)
(require 'ibuffer-setup)
(require 'window-setup)
(require 'workspace-setup)
;; Finish as a editor ;;;;;;;;;;;;;;;;;;;;

;; Terminal
(require 'shell-setup)
;; Writing
(require 'writing-setup)
(require 'util-setup)
;; Finish as a writer;;;;;;;;;;;;;;;;;;;;;

;; Programming
(require 'vcs-setup)
(require 'flycheck-setup)
(require 'lsp-setup)
(require 'progm-setup)
;; Lang
(require 'elisp-setup)
(require 'rust-setup)


(provide 'init)

;;; init.el ends here
