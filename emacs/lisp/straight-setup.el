;; init-base.el --- Package Manage configurations.	-*- lexical-binding: t -*-

;;; Commentery:
;; 重中之重：以下包必须提前安装

;;; Code:

; elpaca 包管理器
;; When on Emacs 29 with the --with-native-compilation turned on, make sure you’re on straight.el’s development branch

;; Install straight.el
;; (setq straight-repository-branch "develop")

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; Integration with use-package
;; (straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
;; (use-package straight
;;   :custom
;;   (straight-use-package-by-default t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'straight-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elpaca-setup.el ends here
