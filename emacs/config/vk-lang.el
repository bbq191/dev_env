;; vk-lang.el --- vk-lang configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Treesitter
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Rainbow mode
(use-package rainbow-mode
  :diminish
  :hook prog-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode))

;; Languages
(use-package lua-mode)

(provide 'vk-lang)

;;; vk-lang.el ends here
