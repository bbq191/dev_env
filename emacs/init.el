;; init.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; use package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (message "refreshing contents ...... ")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)     ;不用每个包都手动添加:ensure t关键字
  (setq use-package-always-defer nil)    ;默认都不是延迟加载
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq package-native-compile t)
  (setq use-package-verbose t))
(eval-when-compile
  (require 'use-package)
  (require 'ob-tangle))

(defun reload-config ()
  "Reload the literate config from ~/.config/emacs/readme.org."
  (interactive)
  (org-babel-load-file "~/.config/emacs/meow.org"))

(setq max-lisp-eval-depth 2000)

(reload-config)
(provide 'init)
;;; init.el ends here
