;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 优化启动垃圾回收机制
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 增加从进程中一次读取的数据量
(setq read-process-output-max (* 100 1024 1024))

;; 添加数据源
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; 配置包安装方式
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; 工程化加载路径
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)



