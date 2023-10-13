;; vk-cape.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; completion extension
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p p" . completion-at-point) ;; capf
         ("M-p p t" . complete-tag)        ;; etags
         ("M-p p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p p h" . cape-history)
         ("M-p p f" . cape-file)
         ("M-p p k" . cape-keyword)
         ("M-p p s" . cape-elisp-symbol)
         ("M-p p e" . cape-elisp-block)
         ("M-p p a" . cape-abbrev)
         ("M-p p l" . cape-line)
         ("M-p p w" . cape-dict)
         ("M-p p \\" . cape-tex)
         ("M-p p _" . cape-tex)
         ("M-p p ^" . cape-tex)
         ("M-p p &" . cape-sgml)
         ("M-p p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(provide 'vk-cape)


;;; vk-cape.el ends here
