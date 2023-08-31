;; init.el --- init configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "custom" dir)))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir)))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "theme" dir))))

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
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))
;; Requisites
(require 'custom-setup)
(require 'func-setup)
;; Package management


;; Modify these package integrated in init
;; (use-package general :ensure t)
;; (elpaca-wait)
;; (use-package no-littering :ensure t)
;; (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory))
;; (setq no-littering-var-directory (expand-file-name "var/" user-emacs-directory))

;; ;; base build-in config
;; (require 'basic-setup)
;; (require 'util-setup)
;; (require 'window-setup)
;; ;; after base config
;; (require 'minibuffer-setup)
;; (require 'completion-setup)
;; (require 'workspace-setup)
;; (require 'lsp-setup)
;; (require 'coding-setup)
;; (require 'org-setup)

;; ;; lang config
;; (require 'markdown)
;; (require 'rust)


(provide 'init)

;;; init.el ends here
