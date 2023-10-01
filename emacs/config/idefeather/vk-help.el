;; vk-help.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Built-in xref and eldoc are powerful packages
(use-package xref
  :pin gnu
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("s-r" . #'xref-find-references)
         ("C-<down-mouse-1>" . #'xref-find-definitions)
         ("C-S-<down-mouse-1>" . #'xref-find-references)
         ("C-<down-mouse-2>" . #'xref-go-back)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t)
  :config
  (use-package eldoc-box
    :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    (eldoc-box-body ((t (:inherit tooltip))))
    :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
    :config
    ;; Prettify `eldoc-box' frame
    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

(use-package flycheck)

(provide 'vk-help)
;;; vk-help.el ends here
