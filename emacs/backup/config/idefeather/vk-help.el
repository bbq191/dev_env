;; vk-help.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Built-in xref and eldoc are powerful packages
(use-package xref
  :pin gnu
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("s-r" . #'xref-find-references)
         ("s-d" . #'xref-find-definitions)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

;; (use-package eldoc
;;   :diminish t
;;   :bind ("s-d" . #'eldoc)
;;   :config
;;   (use-package eldoc-box
;;     :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
;;     :custom-face
;;     (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
;;     (eldoc-box-body ((t (:inherit tooltip))))
;;     :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
;;     :config
;;     ;; Prettify `eldoc-box' frame
;;     (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
;;           (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
              ("M-<f1>" . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))

      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (js2-mode        . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
    Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

(provide 'vk-help)
;;; vk-help.el ends here
