;; vk-eglot.el --- vk-eglot configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; eglot
(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode
                                 'lisp-mode
                                 'makefile-mode
                                 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :config
  (use-package consult-eglot
    :general (:keymaps 'eglot-mode-map
                       "SPC c e s"  #'consult-eglot-symbols)))


(use-package eldoc-box
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom-face
  (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
  :config
  ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))


(provide 'vk-eglot)

;;; vk-eglot.el ends here
