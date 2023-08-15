;;; init.el --- configuration entry point.
;;
;;; Commentary:
;; 从  Prelude 的配置改造而来，请自行 GitHub 查找

;;; Code:

(require 'package)
(require 'cl-lib)

;;; 包路径及数据源
(defconst is-mac (eq system-type 'darwin)) ;; 应以mac判断常量
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))
;; Standard package repositories
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/")
             '("gnu-devel" . "https://elpa.gnu.org/devel/"))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

;; 文件读取存储路径
(defvar root-dir (file-name-directory load-file-name)
  "定义包根路径，为以后拆分文件做准备！！！")
(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "历史文件存储路径")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 500000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 1000000000)

(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)

(require-package 'fullframe)
(fullframe list-packages quit-window)
;; init.el end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 界面及部分核心功能优化
;; 设定界面透明
(defun sanityinc/adjust-opacity (frame incr)
  "调整界面背景透明度-------"
  (unless (display-graphic-p frame)
    (error "当前模式无法调整界面透明度--------"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(when (and is-mac (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; 设置边框及界面
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)
(blink-cursor-mode -1)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
;; mode line settings
;; (line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-nlinum-mode t))
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " 小霸王学习机 - " 
           (:eval (if (buffer-file-name)
                      (abbreviate-file-name (buffer-file-name))
                      "%b"))))
;; 禁用鼠标  
(require-package 'disable-mouse)
;; counsel
(require-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
;; ace-window
(require-package 'ace-window)
(setq aw-background nil)
(global-set-key (kbd "C-x o") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
	(?m aw-swap-window "Swap Windows")
	(?M aw-move-window "Move Window")
	(?c aw-copy-window "Copy Window")
	(?j aw-switch-buffer-in-window "Select Buffer")
	(?n aw-flip-window)
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?c aw-split-window-fair "Split Fair Window")
	(?v aw-split-window-vert "Split Vert Window")
	(?b aw-split-window-horz "Split Horz Window")
	(?o delete-other-windows "Delete Other Windows")
	(?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")
;; avy
(require-package 'avy)
(setq avy-case-fold-search nil)       ;; case sensitive makes selection easier
(bind-key "C-;"    'avy-goto-char-2)  ;; I use this most frequently
(bind-key "C-'"    'avy-goto-line)    ;; Consistent with ivy-avy
(bind-key "M-g c"  'avy-goto-char)
(bind-key "M-g e"  'avy-goto-word-0)  ;; lots of candidates
(bind-key "M-g g"  'avy-goto-line)    ;; digits behave like goto-line
(bind-key "M-g w"  'avy-goto-word-1)  ;; first character of the word
(bind-key "M-g ("  'avy-goto-open-paren)
(bind-key "M-g )"  'avy-goto-close-paren)
(bind-key "M-g P"  'avy-pop-mark)
;; hydra
(require-package 'hydra)

;; 代码相关部分
;; company
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
(setq company-transformers '(company-sort-by-occurrence))

(require-package 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
;; flychecker
(require-package 'flycheck)
(setq truncate-lines nil) ; 如果单行信息很长会自动换行
(global-flycheck-mode)
;; clangd
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;;; init.el ends here
