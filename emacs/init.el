;; init.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; By default, Emacs requires you to hit ESC three times to escape quit the minibuffer.
(global-set-key [escape] 'keyboard-escape-quit)

;; Load `custom-file'
(setq custom-file (expand-file-name "vk-custom.el" user-emacs-directory))

(let ((dir (locate-user-emacs-file "config")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; GUI frame init
(require 'vk-frame)
(require 'vk-font)
(require 'vk-elpaca)
(require 'vk-theme)

;; Remap keys
(require 'vk-keybind)

;; Minibuffer
(require 'vk-dired)
(require 'vk-recentf)
(require 'vk-isearch)
(require 'vk-nerdicon)
(require 'vk-vertico)
(require 'vk-minibuffer)

;; Completion
(require 'vk-orderless)
(require 'vk-corfu)
(require 'vk-cape)

;; Programe
(require 'vk-lsp)
(require 'vk-flycheck)
(require 'vk-git)

;; Language
(require 'vk-lang)
(require 'vk-rustic)

;; Util
(require 'vk-util)

(provide 'init)

;;; init.el ends here
