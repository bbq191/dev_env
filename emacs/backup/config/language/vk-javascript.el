;; vk-javascript.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package typescript-mode
  :custom (typescript-indent-level 2))
(setq-default js-indent-level 2)

(use-package js2-mode
  :hook (js2-mode . js2-imenu-extras-mode)
  :mode ("\\.js$" . js2-mode)
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t))

(use-package xref-js2
  :ensure t
  :hook (js2-mode . pt/js-hook)
  :custom
  (xref-js2-search-program 'rg)
  :config
  (defun pt/js-hook ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package json-mode)
(use-package prettier-js)


(provide 'vk-javascript)
;;; vk-javascript.el ends here
