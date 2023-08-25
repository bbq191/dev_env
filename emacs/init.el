;;; Code:

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

;; Bootstrap config
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

;; 必须提前所有包之前加载的两个配置，org 也必须提前是因为后面使用 org-babel-load-file，
;; 如果不提前加载好 org 包会造成和 emacs 的 build-in 的 org 包冲突
(require 'elpaca-setup)
(require 'basic-setup)
(require 'func-setup)
(require 'util-setup)

(provide 'init)

;;; init.el ends here
