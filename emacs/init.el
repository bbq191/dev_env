;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap config
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

;; 必须提前所有包之前加载的两个配置，org 也必须提前是因为后面使用 org-babel-load-file，
;; 如果不提前加载好 org 包会造成和 emacs 的 build-in 的 org 包冲突
(require 'elpaca-setup)

(org-babel-load-file (expand-file-name "lisp/basic.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "lisp/org.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "lisp/funcs.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "lisp/theme.org" user-emacs-directory))

(provide 'init)

;;; init.el ends here
