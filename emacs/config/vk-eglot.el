;; vk-eglot.el --- vk-eglot configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode
                                 'lisp-mode
                                 'makefile-mode
                                 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)))

(provide 'vk-eglot)

;;; vk-eglot.el ends here
