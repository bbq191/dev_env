;; -*- coding: utf-8; lexical-binding: t -*-

(setq gc-cons-threshold 100000000)
(setq max-specpdl-size 5000)

(bind-key* "C-c ;" #'execute-extended-command)
(bind-key* "C-c 4" #'execute-extended-command) ;; for a purely left-handed combo
(bind-key* "C-c C-;" #'execute-extended-command-for-buffer)

;; exec-path-from shell was misbehaving, this hack seems to mollify it
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-copy-env "HTTPS_PROXY")
  (exec-path-from-shell-copy-env "HTTP_PROXY")
  (exec-path-from-shell-copy-env "ALL_PROXY")
  (exec-path-from-shell-copy-env "LLVM_HOME")
  (exec-path-from-shell-copy-env "LDFLAGS")
  (exec-path-from-shell-copy-env "CPPFLAGS")
  (exec-path-from-shell-copy-env "CARGO_HOME")
  (exec-path-from-shell-copy-env "RUSTUP_HOME"))

(use-package use-package-ensure-system-package)

;; Title
(setq frame-title-format '("Vinci & Kate's Gnu Emacs - %b")
      icon-title-format frame-title-format)

;; Set proxy configurations in emacs
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|127.0.0.1\\)")
        ("http" . "localhost:6152")
        ("https" . "localhost:6152")))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil     ; Permanently indent with spaces, never with TABs
              truncate-lines t
              display-line-numbers-width 3
              indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?\u254e)

(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      blink-cursor-mode nil             ; No eyes distraction
      column-number-mode t
      create-lockfiles nil
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil     ; Double-spaces after periods is morally wrong.
      word-wrap-by-category t
      use-short-answers t
      mark-even-if-inactive nil         ; Fix undo in commands affecting the mark.
      ;; Suppress GUI features
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Let C-k delete the whole line.
      kill-whole-line t
      ;; search should be case-sensitive by default
      case-fold-search nil
      ;; I want to close these fast, so switch to it so I can just hit 'q'
      help-window-select t
      ;; highlight error messages more aggressively
      next-error-message-highlight t
      ;; don't let the minibuffer muck up my window tiling
      read-minibuffer-restore-windows t
      ;; don't let the minibuffer muck up my window tiling
      read-minibuffer-restore-windows t
      ;; scope save prompts to individual projects
      save-some-buffers-default-predicate 'save-some-buffers-root
      ;; don't keep duplicate entries in kill ring
      kill-do-not-save-duplicates t
      truncate-string-ellipsis "…"  ;; unicode ellipses are better
      custom-safe-themes t
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      ;; eke out a little more scrolling performance
      fast-but-imprecise-scrolling t
      ;; prefer newer elisp files
      load-prefer-newer t
      ;; more info in completions
      completions-detailed t
      ;; 允许在活动的minibuffer中执行命令并打开新的minibuffer。这样可以实现命令的嵌套。
      enable-recursive-minibuffers t
      ;; Some pretty config from prucell
      initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(delete-selection-mode t)    ;; You can select text and delete it by typing.
(savehist-mode)
(minibuffer-depth-indicate-mode) ;;开头显示当前嵌套层级的深度,用方括号括起,以示区分
;; UTF-8 should always, always be the default.
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; Emacs has problems with very long lines.
(global-so-long-mode)

;; URLs should be highlighted and linkified.
(global-goto-address-mode)

;; Display wrape line
(global-display-fill-column-indicator-mode 1)
(global-visual-line-mode 1)

;; Show line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Eemacs true transparent
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Emacs requires you to hit ESC three times to escape quit the minibuffer.
(global-set-key [escape] 'keyboard-escape-quit)

;; Fonts
(defun vk/setup-fonts ()
  (set-face-attribute 'default nil
                      :family "Iosevka Fixed"
                      :height 150)

  (set-fontset-font t 'symbol (font-spec :family "Nerd Font Symbol Mono") nil 'prepend)
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font t 'han (font-spec :family "Source Han Sans CN"))

  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(vk/setup-fonts)
(add-hook 'window-setup-hook #'vk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'vk/setup-fonts)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        mac-right-command-modifier 'super))

(use-package diminish
  :config (diminish 'visual-line-mode))

(use-package no-littering
  ;; After no-littering
  ;; Set user custom
  :config
  (setq custom-file (no-littering-expand-etc-file-name "vk-custom.el")))

(use-package hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(defun vk/unbind-bad-keybindings ()
  "Remove unhelpful keybindings."
  (-map (lambda (x) (unbind-key x))
        '("C-x C-r"         ;; find-file-read-only
          "C-x C-f"         ;; find-file
          "C-x C-d"         ;; list-directory
          "C-z"             ;; suspend-frame
          "C-x C-z"         ;; again
          "<mouse-2>"       ;; pasting with mouse-wheel click
          "<C-wheel-down>"  ;; text scale adjust
          "<C-wheel-up>"    ;; ditto
          "s-l"             ;; goto-line
          "s-w"             ;; delete-frame
          "s-n"             ;; make-frame
          "s-t"             ;; ns-popup-font-panel
          "s-p"             ;; ns-print-buffer
          "C-x C-q"         ;; read-only-mode
          "C-x C-c"         ;; quit emacs
          "C-h")))          ;; help

(use-package s)
(use-package dash :config (vk/unbind-bad-keybindings))
(use-package shut-up)

(bind-key* "C-h" #'backward-delete-char)
(bind-key* "M-h" #'backward-delete-word)
(bind-key* "C-c C-h k" #'describe-key)
(bind-key* "C-c C-h f" #'describe-function)
(bind-key* "C-c C-h m" #'describe-mode)
(bind-key* "C-c C-h v" #'describe-variable)
(bind-key* "C-c C-h l" #'view-lossage)

(bind-key "s-<up>" #'ff-find-related-file)
(bind-key "C-c a f" #'ff-find-related-file)

(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-c s" #'isearch-forward-symbol)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)
(bind-key "C-c q" #'fill-paragraph)
(bind-key "C-c Q" #'set-fill-column)

(defun pt/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'pt/indent-just-yanked)

(use-package vundo
  :diminish
  :bind* (("C-c _" . vundo))
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(setq enable-local-variables :all)

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
(setq-default truncate-lines t)

(use-package dabbrev
  :bind* (("C-/" . #'dabbrev-completion))
  :custom
  (dabbrev-check-all-buffers t)
  (dabbrev-case-replace nil))

(add-hook 'prog-mode-hook #'abbrev-mode)
(setq abbrev-suggest t)

(defun check-config ()
  "Warn if exiting Emacs with a readme.org that doesn't load."
  (or
   (ignore-errors (org-babel-load-file "~/.config/emacs/readme.org"))
   (y-or-n-p "Configuration file may be malformed: really exit?")))

(push #'check-config kill-emacs-query-functions)

(use-package fancy-compilation :config (fancy-compilation-mode))

;; Icons
(use-package nerd-icons :defer nil)

(use-package nerd-icons-dired
  :defer nil
  :diminish t
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :defer nil
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :defer nil
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init (setq nerd-icons-ibuffer-icon t))

;; For treemacs
(use-package treemacs-nerd-icons
  :disabled
  :defer nil
  :config
  (treemacs-load-theme "nerd-icons"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package doom-themes
  :demand t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (let ((chosen-theme 'tango-dark))
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-rouge-brighter-comments t
          doom-ir-black-brighter-comments t
          modus-themes-org-blocks 'gray-background
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme t)))

(defun pt/project-relative-file-name (include-prefix)
  "Return the project-relative filename, or the full path if INCLUDE-PREFIX is t."
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (if fullname (file-relative-name fullname root) fullname))
       (should-strip (and root (not include-prefix))))
    (if should-strip relname fullname)))

(use-package mood-line
  :config
  (defun pt/mood-line-segment-project-advice (oldfun)
    "Advice to use project-relative file names where possible."
    (let ((project-relative (ignore-errors (pt/project-relative-file-name nil))))
      (if (and (project-current) project-relative)
          (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
        (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'pt/mood-line-segment-project-advice)
  (mood-line-mode))

(use-package keycast
  :commands (+toggle-keycast)
  :config
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON")))
  :hook (after-init . +toggle-keycast))

(use-package centered-window
  :custom
  (cwm-centered-window-width 180))

(add-hook 'compilation-mode-hook 'visual-line-mode)

(shut-up
  (use-package tree-sitter
    :config (global-tree-sitter-mode))

  (use-package tree-sitter-langs))

(use-package centaur-tabs
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-gray-out-icons t)

  :bind
  (("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward)))

(use-package multiple-cursors
  :bind (("C-c C-e m" . #'mc/edit-lines)
         ("C-c C-e d" . #'mc/mark-all-dwim)))

(use-package expand-region
  :bind (("C-c n" . er/expand-region)))

(bind-key* "C-c /" #'comment-dwim)
(bind-key* "C-c 0" #'capitalize-dwim)

(use-package avy
  :bind (:map prog-mode-map ("C-'" . #'avy-goto-line))
  :bind (:map org-mode-map ("C-'" . #'avy-goto-line))
  :bind (("C-c l" . #'avy-goto-line)
         ("C-c j k" . #'avy-kill-whole-line)
         ("C-c j h" . #'avy-kill-region)
         ("C-c j w" . #'avy-copy-line)
         ("C-z" . #'avy-goto-char)
         ("C-c v" . #'avy-goto-char)))

(use-package avy-zap
  :bind (("C-c z" . #'avy-zap-to-char)
         ("C-c Z" . #'avy-zap-up-to-char)))

(shut-up (use-package iedit
           :bind (:map iedit-mode-keymap ("C-h" . #'sp-backward-delete-char))
           :bind ("C-;" . #'iedit-mode)))

(use-package smartparens
  :bind (("C-(" . #'sp-backward-sexp)
         ("C-)" . #'sp-forward-sexp)
         ("C-c d w" . #'sp-delete-word)
         ("<left>" . #'sp-backward-sexp)
         ("<right>" . #'sp-forward-sexp)
         ("C-c C-(" . #'sp-up-sexp)
         ("C-c j s" . #'sp-copy-sexp)
         ("C-c C-)" . #'sp-down-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0
        sp-show-pair-from-inside t)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  ;; (set-face-attribute 'sp-pair-overlay-face nil :background "#0E131D")
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(use-package nameless
  :custom
  (nameless-private-prefix t))

(defun pt/eol-then-newline ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(bind-key "s-<return>" #'pt/eol-then-newline)

(bind-key "C-c U" #'insert-char)

(defun pt/split-window-thirds ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(bind-key "C-c 3" #'pt/split-window-thirds)

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.config/emacs/readme.org"))

(bind-key "C-c E" #'open-init-file)

(defun pt/insert-current-date ()
  "Insert the current date (Y-m-d) at point."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(bind-key "s-w" #'kill-this-buffer)

(defun pt/check-file-modification (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (not (file-remote-p buffer-file-name)) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (autoload 'diff-no-select "diff")
  (when buffer-file-name
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

;; (advice-add 'save-some-buffers :before #'pt/check-file-modification)

;; (add-hook 'before-save-hook #'pt/check-file-modification)
;; (add-hook 'kill-buffer-hook #'pt/check-file-modification)
(advice-add 'magit-status :before #'pt/check-file-modification)
(advice-add 'save-buffers-kill-terminal :before #'pt/check-file-modification)

(use-package sudo-edit)

(setq
 ;; I use exa, which doesn't have a --dired flag
 dired-use-ls-dired nil
 ;; Why wouldn't you create destination directories when copying files, Emacs?
 dired-create-destination-dirs 'ask
 ;; Before the existence of this option, you had to either hack
 ;; dired commands or use the dired+ library, the maintainer
 ;; of which refuses to use a VCS. So fuck him.
 dired-kill-when-opening-new-dired-buffer t
 ;; Update directory listings automatically (again, why isn't this default?)
 dired-do-revert-buffer t
 ;; Sensible mark behavior
 dired-mark-region t
 )

(use-package dired-recent :config (dired-recent-mode))

(global-so-long-mode)

(use-package duplicate-thing
  :init
  (defun pt/duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1))
    (call-interactively #'next-line))
  :bind (("C-c u" . pt/duplicate-thing)
         ("C-c C-u" . pt/duplicate-thing)))

(require 're-builder)
(setq reb-re-syntax 'string)

(setq read-process-output-max (* 1024 1024)) ; 1mb

(use-package which-key
  :init (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 4
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.15
        which-key-idle-delay 1.5
        which-key-max-description-length 40
        which-key-separator " │→ " ))

(defun display-startup-echo-area-message ()
  "Override the normally tedious startup message."
  (message "Welcome back."))

(setq executable-prefix-env t)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(context-menu-mode)
(bind-key "C-c C-m" #'tmm-menubar)

(defun revert-to-two-windows ()
  "Delete all other windows and split it into two."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(bind-key "C-x 1" #'revert-to-two-windows)
(bind-key "C-x !" #'delete-other-windows) ;; Access to the old keybinding.

(defun pt/abort ()
  "Remove auxiliary buffers."
  (interactive)
  (ignore-errors (exit-recursive-edit))
  (ignore-errors (ctrlf-cancel))
  (popper-close-latest)
  (call-interactively #'keyboard-quit))

(bind-key* "s-g" #'pt/abort)

(defun kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (pt/check-file-modification)
  (kill-buffer nil)
  )

(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-x K" #'kill-buffer)

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (let ((lsp-restart 'ignore))
    ;; (maybe-unset-buffer-modified)
    (delete-other-windows)
    (save-some-buffers)
    (let
        ((kill-buffer-query-functions '()))
      (mapc 'kill-buffer (buffer-list)))))

(bind-key "C-c K" #'kill-all-buffers)

(defun copy-file-name-to-clipboard (do-not-strip-prefix)
  "Copy the current buffer file name to the clipboard. The path will be relative to the project's root directory, if set. Invoking with a prefix argument copies the full path."
  (interactive "P")
  (let
      ((filename (pt/project-relative-file-name do-not-strip-prefix)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(bind-key "C-c p" #'copy-file-name-to-clipboard)

(use-package ace-window
  :config
  ;; Show the window designators in the modeline.
  (ace-window-display-mode)

  :bind* (("C-<" . other-window) ("C-," . ace-window) ("C-c ," . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Designate windows by home row keys, not numbers.")
  (aw-background nil))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(bind-key "C-c a s" #'switch-to-scratch-buffer)

(use-package popper
  :bind* ("C-c :" . popper-toggle-latest)
  :bind (("C-`"   . popper-toggle-latest)
         ("C-\\"  . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :hook (prog-mode . popper-mode)
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-window-height 24)
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              "\\*rustic-compilation\\*"
                              help-mode
                              prodigy-mode
                              "magit:.\*"
                              "\\*deadgrep.\*"
                              "\\*eldoc.\*"
                              "\\*Codespaces\\*"
                              "\\*SCLang:PostBuffer\\*"
                              "\\*xref\\*"
                              "\\*org-roam\\*"
                              "\\*direnv\\*"
                              "\\*tidal\\*"
                              "\\*Checkdoc Status\\*"
                              "\\*Warnings\\*"
                              "\\*Go Test\\*"
                              "\\*Bookmark List\\*"
                              haskell-compilation-mode
                              compilation-mode
                              bqn-inferior-mode)))
