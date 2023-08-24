;;; Code:

(add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
