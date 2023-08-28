;;; Code:

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir)))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "theme" dir))))

;; Bootstrap config
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Load `custom-file'
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it by the
;; `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' benefits from that.
(setq read-process-output-max (* 512 1024 1024))

;; Setup proxy TODO: Add en/disable toggle
(setq url-proxy-services
      '(("http" . "127.0.0.1:6152")
        ("https" .  "127.0.0.1:6152")
        ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
;; (require 'package)
;; (setq package-archives
;;       '(("melpa"  . "https://melpa.org/packages/")
;;         ("gnu"    . "https://elpa.gnu.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-and-compile
;;   (setq use-package-always-ensure nil)
;;   (setq use-package-always-defer nil)
;;   (setq use-package-always-demand nil)
;;   (setq use-package-expand-minimally nil)
;;   (setq use-package-enable-imenu-support t))
;; (eval-when-compile
;;   (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "etc/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; The elpaca-example macro in the following examples reduces verbosity. It is not part of Elpaca.
(defun elpaca-example-sort-plist (plist)
  "Return copy of PLIST with :package followed by lexically sorted key/val pairs."
  `(:package ,(plist-get plist :package)
             ,@(cl-loop for k in (cl-sort (cl-loop for key in plist by #'cddr
                                                   unless (eq key :package) collect key)
                                          #'string< :key #'symbol-name)
                        append (list k (plist-get plist k)))))

(defmacro elpaca-example (&rest body)
  "Execute BODY with a clean elpaca environment."
  `(let (elpaca-cache-menu-items
         elpaca-order-functions
         elpaca-recipe-functions
         (elpaca-menu-functions '(elpaca-example-menu)))
     (elpaca-example-sort-plist ,@body)))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
;; Block until current queue processed.
(elpaca-wait)

(use-package no-littering :ensure t  )
(elpaca-wait)
(setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory))
(setq no-littering-var-directory (expand-file-name "var/" user-emacs-directory))


;; 必须提前所有包之前加载的两个配置，org 也必须提前是因为后面使用 org-babel-load-file，
;; 如果不提前加载好 org 包会造成和 emacs 的 build-in 的 org 包冲突
;; (require 'elpaca-setup)
(require 'basic-setup)
(require 'func-setup)
(require 'theme-setup)

;; after base config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'util-setup)
(require 'minibuffer-setup)
(require 'completion-setup)
(require 'lsp-setup)
(require 'coding-setup)

;; lang config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown)

(provide 'init)

;;; init.el ends here
