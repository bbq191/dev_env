;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Better defaults.
;;
;;; Code:

;; 变更默认参数
;; 基本配置设定，改变一些必要的默认参数
(setq-default use-short-answers t
              fill-column 90
              frame-resize-pixelwise t
              window-resize-pixelwise t
              gc-cons-threshold most-positive-fixnum ; Defer garbage collection further back in the startup process
              auto-mode-case-fold nil       ;; Don't pass case-insensitive to `auto-mode-alist'
              tab-width 4
              display-line-numbers-width 3
              indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?\u254e
              indent-tabs-mode nil              ; Permanently indent with spaces, never with TABs
              visible-bell t
              inhibit-compacting-font-caches t  ; Don't compact font caches during GC
              delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
              make-backup-files nil             ; Forbide to make backup files
              auto-save-default nil             ; Disable auto save
              create-lockfiles nil
              ring-bell-function 'ignore        ; No annoying bell
              blink-cursor-mode nil             ; No eyes distraction
              uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
              adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*" ; 设置自动换行的正则表达式，这里是断行在多个空格或制表符后
              adaptive-fill-first-line-regexp "^* *$" ; 设置首行的自动换行正则表达式，这里是在注释开头断行
              sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*" ; 设置句子的结束标点正则表达式，这里包括中文和英文的常见结束标点
              sentence-end-double-space nil ; 句子结束后使用两个空格而不是一个
              word-wrap-by-category t ;按词类换行而不是按单词换行
              truncate-lines nil
              truncate-partial-width-windows nil
              save-interprogram-paste-before-kill t ; 在程序间粘贴前保存内容
              use-file-dialog nil ; 关闭使用系统自带的文件选择对话框,使用Emacs自带的。
              use-dialog-box nil ; 关闭使用系统自带的消息框,使用Emacs自带的。
              inhibit-startup-screen t ; 阻止显示启动画面。
              inhibit-startup-message t ; 阻止显示启动消息。
              inhibit-startup-buffer-menu t ; 阻止显示启动缓冲区菜单。
              bidi-paragraph-direction 'left-to-right   ; 修改双向文字排版为从左到右
              bidi-inhibit-bpa t
              line-spacing 0.12
              initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; 模式命令
;; 模式命令设定，改变一些基本模式
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
;; (desktop-save-mode 1)
(save-place-mode 1)
(delete-selection-mode 1)
;; basic ui
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; hist setting
(savehist-mode 1)
(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)


;; Const
;; 判断是否是 macOS
(defconst is-macsys (eq system-type 'darwin))

;; Function
;; Newline behaviour
(defun vk/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
;; 按键绑定
(global-set-key (kbd "S-<return>") 'vk/newline-at-end-of-line)

;; Reload Init
(defun vk/reload-init-file ()
  "需要两次 load-file，否则不生效。"
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))
(global-set-key (kbd "C-c a r") 'vk/reload-init-file)

;; Adjust Opacity - This function from purcell.
(defun vk/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
;; 调整界面 opacity
(global-set-key (kbd "M-C-8") (lambda () (interactive) (ikate/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (ikate/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


;; about shell
(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(provide 'basic-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
