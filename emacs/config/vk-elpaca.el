;; vk-elpaca.el --- init configurations. -*- lexical-binding: t -*-j
;;; Commentary:
;;
;;; Code:

;; Package setup and additional utility functions
(setq read-process-output-max (* 4 1024 1024))

(require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile 
    (setq use-package-always-ensure t) ;不用每个包都手动添加:ensure t关键字 
    (setq use-package-always-defer t) ;默认都是延迟加载，不用每个包都手动添加:defer t 
    (setq use-package-always-demand nil) 
    (setq use-package-expand-minimally t) 
    (setq use-package-verbose t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering)

;; Bootstrap `quelpa'.
(use-package quelpa
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; Keep modeline clean.
(use-package diminish)

;; MacOS specific
(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

;; Set user custom
(setq custom-file (no-littering-expand-etc-file-name "vk-custom.el"))

(provide 'vk-elpaca)

;;; vk-elpaca.el ends here
