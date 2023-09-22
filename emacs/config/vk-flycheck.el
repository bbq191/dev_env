;; vk-flycheck.el --- vk-flycheck configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package flymake
  :diminish
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path)))

(use-package sideline-flymake
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(provide 'vk-flycheck)

;;; vk-flycheck.el ends here
