;; vk-elpaca.el --- init configurations. -*- lexical-binding: t -*-j
;;; Commentary:
;;
;;; Code:

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if init-file-debug
    (setq use-package-verbose t
          use-package-always-ensure t
	      use-package-always-demand t
          use-package-always-defer nil
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update :ensure t)

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Keep ~/.emacs.d/ clean.
(use-package no-littering :ensure t)

;; Keep modeline clean.
(use-package diminish :ensure t)

;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'vk-elpaca)

;;; vk-elpaca.el ends here
