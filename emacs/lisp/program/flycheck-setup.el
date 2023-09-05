;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(use-package flymake
  :diminish
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path)))

(use-package sideline-flymake
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

;; Flycheck and extensions
(use-package flycheck
  ;;   :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

;; ;; Flycheck Extensions
;; ;; inline
;; (use-package flycheck-inline
;;   :after flycheck
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; for rust
(use-package flycheck-rust
  :after rustic-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'flycheck-setup)

;;; flycheck-setup.el ends here
