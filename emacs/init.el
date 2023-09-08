;; init.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; --debug-init implies `debug-on-error'
(setq init-file-debug t)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Load `custom-file'
(setq custom-file (expand-file-name "vk-custom.el" user-emacs-directory))

(let ((dir (locate-user-emacs-file "config")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; GUI frame init
(require 'vk-frame)
(require 'vk-elpaca)

;; Remap keys
(require 'vk-evil)
(require 'vk-keybind)

;; Minibuffer
(require 'vk-dired)
(require 'vk-marginalia)
(require 'vk-nerdicon)
(require 'vk-vertico)

;; Completion
(require 'vk-orderless)
(require 'vk-corfu)


(provide 'init)

;;; init.el ends here
