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
                      :height 140)

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
  :disabled
  :demand t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (let ((chosen-theme 'doom-Iosvkem))
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-rouge-brighter-comments t
          doom-ir-black-brighter-comments t
          modus-themes-org-blocks 'gray-background
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme t)))

(use-package tron-legacy-theme
  :config
  (setq tron-legacy-theme-dark-fg-bright-comments t)
  (setq tron-legacy-theme-vivid-cursor t)
  (load-theme 'tron-legacy t))

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
 dired-mark-region t)

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
  (kill-buffer nil))

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

(use-package org
  :hook ((org-mode . visual-line-mode) (org-mode . pt/org-mode-hook))
  :hook ((org-src-mode . display-line-numbers-mode)
         (org-src-mode . pt/disable-elisp-checking))
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         ("C-c o A" . consult-org-agenda)
         :map org-mode-map
         ("M-<left>" . nil)
         ("M-<right>" . nil)
         ("C-c c" . #'org-mode-insert-code)
         ("C-c a f" . #'org-shifttab)
         ("C-c a S" . #'zero-width))
  :custom
  (org-adapt-indentation nil)
  (org-directory "~/Documents/orgnote")
  (org-special-ctrl-a/e t)

  (org-default-notes-file (concat org-directory "/note"))
  (org-return-follows-link t)
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  ;; (org-src-window-setup 'current-window)
  (org-agenda-files (list (concat org-directory "/todo")))
  (org-pretty-entities t)

  :config
  (defun pt/org-mode-hook ())
  (defun make-inserter (c) '(lambda () (interactive) (insert-char c)))
  (defun zero-width () (interactive) (insert "​"))

  (defun pt/disable-elisp-checking ()
    (flymake-mode nil))
  (defun org-mode-insert-code ()
    "Like markdown-insert-code, but for org instead."
    (interactive)
    (org-emphasize ?~)))

(use-package org-modern
  :config (global-org-modern-mode)
  :custom (org-modern-variable-pitch nil))

(use-package org-ref
  :disabled ;; very slow to load
  :config (defalias 'dnd-unescape-uri 'dnd--unescape-uri))

(use-package org-roam
  :bind
  (("C-c o r" . #'org-roam-capture)
   ("C-c o f" . #'org-roam-node-find)
   ("C-c o t" . #'org-roam-tag-add)
   ("C-c o i" . #'org-roam-node-insert)
   ("C-c o :" . #'org-roam-buffer-toggle))
  :custom
  (org-roam-directory (expand-file-name "~/Documents/orgnote/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode))

(use-package org-alert
  :config (org-alert-enable)
  :custom (alert-default-style 'osx-notifier))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package ob-mermaid)

(bind-key "<f12>" #'other-window)

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-diff-refine-hunk t)
  (magit-repository-directories '(("~/Workspace" . 1)))
  (magit-list-refs-sortby "-creatordate")
  :config
  (defun pt/commit-hook () (set-fill-column 80))
  (add-hook 'git-commit-setup-hook #'pt/commit-hook)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; Magit also allows integration with GitHub and other such forges
(use-package forge
  :after magit)

(use-package git-timemachine
  :disabled
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
     (delete . " ")
     (change . " ")
     (unknown . "?")
     (ignored . "i"))))

(use-package emojify)

(use-package code-review
  :custom
  (forge-owned-accounts '(("afu" . nil)))
  (code-review-auth-login-marker 'forge)
  (code-review-fill-column 80)
  (code-review-new-buffer-window-strategy #'switch-to-buffer-other-window)
  :after (magit forge emojify)
  :bind (:map forge-pullreq-section-map (("RET" . #'forge-browse-dwim)
                                         ("C-c r" . #'code-review-forge-pr-at-point)))
  :bind (:map forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
  :bind (:map code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
                                    ("N" . #'code-review-comment-jump-next)
                                    ("P" . #'code-review-comment-jump-previous)
                                    ("C-c p" . #'code-review-comment-jump-previous))))

(use-package compile
  :custom
  (compilation-read-command nil "Don't prompt every time.")
  (compilation-scroll-output 'first-error))

(use-package project
  :pin gnu
  :bind (("C-c k" . #'project-kill-buffers)
         ("C-c m" . #'project-compile)
         ("C-x f" . #'find-file)
         ("C-c F" . #'project-switch-project)
         ("C-c R" . #'pt/recentf-in-project)
         ("C-c f" . #'project-find-file))
  :custom
  ;; This is one of my favorite things: you can customize
  ;; the options shown upon switching projects.
  (project-switch-commands
   '((project-find-file "Find file")
     (magit-project-status "Magit" ?g)
     (deadgrep "Grep" ?h)
     (pt/project-run-vterm "vterm" ?t)
     (project-dired "Dired" ?d)
     (pt/recentf-in-project "Recently opened" ?r)))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil))

(defun pt/recentf-in-project ()
  "As `recentf', but filtering based on the current project root."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Not in a project"))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))

(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init (defface posframe-border
          `((t (:inherit region)))
          "Face used by the `posframe' border."
          :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(use-package vertico
  :demand
  :init
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :config
  (vertico-mouse-mode)
  (set-face-attribute 'vertico-mouse nil :inherit nil)
  (savehist-mode)
  :custom
  (vertico-count 22)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-'"           . vertico-quick-exit)
              ("C-c '"         . vertico-quick-insert)
              ("<return>"      . exit-minibuffer)
              ("C-m"           . vertico-insert)
              ("C-c SPC"       . vertico-quick-exit)
              ("C-<backspace>" . vertico)
              ("DEL"           . vertico-directory-delete-char)))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8)
                (right-fringe . 8))))

(use-package orderless
  :custom
  (completion-styles '(orderless))
                                        ; I want to be in control!
  (completion-category-defaults nil)
  (completion-category-overrides
                                        ; For `tramp' hostname completion with `vertico'
   '((file (styles basic-remote
                   orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher))

  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
  The characters in COMPONENT must occur in the candidate in that
  order at the beginning of subsequent words comprised of letters.
  Only non-letters can be in between the words that start with the
  initials.

  If ANCHORED is `start' require that the first initial appear in
  the first word of the candidate.  If ANCHORED is `both' require
  that the first and last initials appear in the first and last
  words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
  This means the characters in COMPONENT must occur in the
  candidate in that order at the beginning of subsequent words
  comprised of letters.  Only non-letters can be in between the
  words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
  It matches PATTERN _INDEX and _TOTAL according to how Orderless
  parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
  It matches PATTERN _INDEX and _TOTAL according to how Orderless
  parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
  It matches PATTERN _INDEX and _TOTAL according to how Orderless
  parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

;; embark is a cool package for discoverability.
(use-package embark
  :after vertico
  :bind (:map vertico-map
              ("C-c e" . embark-export)
              ("C-<escape>" . embark-act)))
(use-package consult
  :bind* (("C-c r"     . consult-recent-file))
  :bind (("C-c i"     . consult-imenu)
         ("C-c b"     . consult-project-buffer)
         ("C-x b"     . consult-buffer)
         ("C-c B"     . consult-bookmark)
         ("C-c `"     . flymake-goto-next-error)
         ("C-c h"     . consult-ripgrep)
         ("C-c y"     . consult-yank-pop)
         ("C-x C-f"   . find-file)
         ("C-c C-h a" . describe-symbol))
  :custom
  (consult-narrow-key (kbd ";"))
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
  (consult-preview-key '(:debounce 0.25 any)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

(use-package embark-vc :after embark)
(use-package consult-flycheck)

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package ctrlf
  :config (ctrlf-mode))

(use-package prescient
  :config (prescient-persist-mode))

(use-package dumb-jump
  :config
  (defun pt/quietly-dumb-jump ()
    (interactive)
    (shut-up (call-interactively 'dumb-jump-go)))
  :bind (("C-c J" . #'pt/quietly-dumb-jump)))

(use-package deadgrep
  :ensure-system-package rg
  :bind (("C-c H" . #'deadgrep)))

(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

(bind-key* "C-." #'completion-at-point)

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         (lsp-completion-mode . kb/corfu-setup-lsp)) ; Use corfu for lsp completion
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-popupinfo-toggle))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 1)
  (corfu-popupinfo-delay '(2 . 1))

  (corfu-min-width 40)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Other
  (corfu-popupinfo-documentation nil)
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions

  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p p" . completion-at-point) ;; capf
         ("M-p p t" . complete-tag)        ;; etags
         ("M-p p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p p h" . cape-history)
         ("M-p p f" . cape-file)
         ("M-p p k" . cape-keyword)
         ("M-p p s" . cape-elisp-symbol)
         ("M-p p e" . cape-elisp-block)
         ("M-p p a" . cape-abbrev)
         ("M-p p l" . cape-line)
         ("M-p p w" . cape-dict)
         ("M-p p \\" . cape-tex)
         ("M-p p _" . cape-tex)
         ("M-p p ^" . cape-tex)
         ("M-p p &" . cape-sgml)
         ("M-p p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package dap-mode
  :after dap-mode
  :bind
  (:map dap-mode-map
        ("C-c b b" . dap-breakpoint-toggle)
        ("C-c b r" . dap-debug-restart)
        ("C-c b l" . dap-debug-last)
        ("C-c b d" . dap-debug))
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  ;; installs .extension/vscode
  (require 'dap-codelldb)
  ;; (dap-gdb-lldb-setup)

  (setq dap-auto-configure-features '(sessions locals controls tooltip))

  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "Rust::Debug"
         :miDebuggerPath "~/.local/share/cargo/bin/rust-lldb"
         :program: "${workspaceRoot}/target/debug/${fileBasenameNoExtension}")))

(use-package xref
  :pin gnu
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("s-r" . #'xref-find-references)
         ("s-d" . #'xref-find-definitions)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("C-M-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t)
  :config
  (use-package eldoc-box
    :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    ;; (eldoc-box-body ((t (:inherit tooltip))))
    ;; :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
    :config
    ;; Prettify `eldoc-box' frame
    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

(use-package eglot
  :disabled
  :hook ((c++-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-<down-mouse-1>" . #'xref-find-definitions)
              ("C-S-<down-mouse-1>" . #'xref-find-references)
              ("C-c a r" . #'eglot-rename)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  :config
  ;; C++
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  ;; Eglot doesn't correctly unescape markdown: https://github.com/joaotavora/eglot/issues/333
  (defun mpolden/gfm-unescape-string (string)
    "Remove backslash-escape of punctuation characters in STRING."
    ;; https://github.github.com/gfm/#backslash-escapes
    (replace-regexp-in-string "[\\\\]\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)" "\\1" string))

  (advice-add 'eglot--format-markup :filter-return 'mpolden/gfm-unescape-string)

  (defun pt/add-eglot-to-prog-menu (old startmenu click)
    "Add useful Eglot functions to the prog-mode context menu."
    (let ((menu (funcall old startmenu click))
          (identifier (save-excursion
                        (mouse-set-point click)
                        (xref-backend-identifier-at-point
                         (xref-find-backend)))))
      (when identifier
        (define-key-after menu [eglot-find-impl]
          `(menu-item "Find Implementations" eglot-find-implementation
                      :help ,(format "Find implementations of `%s'" identifier))
          'xref-find-ref))
      menu))

  (advice-add 'prog-context-menu :around #'pt/add-eglot-to-prog-menu))

(use-package consult-eglot
  :config
  (defun pt/consult-eglot ()
    (interactive)
    (let ((completion-styles '(emacs22)))
      (call-interactively #'consult-eglot-symbols)))
  :bind (:map eglot-mode-map ("s-t" . #'pt/consult-eglot)))

(use-package lsp-mode
  :commands (lsp-format-buffer lsp-organize-imports)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable nil
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate nil
        lsp-semantic-tokens-enable t)

  :config
  (use-package consult-lsp
    :bind (:map lsp-mode-map
                ("C-M-." . consult-lsp-symbols))))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c U" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-keymap-prefix "s-l"
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 1
        lsp-ui-doc-show-with-cursor (not (display-graphic-p))
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (-let* ((win-width (frame-width))
              (lsp-ui-peek-list-width (/ (frame-width) 2))
              (string (-some--> (-zip-fill "" src1 src2)
                        (--map (lsp-ui-peek--adjust win-width it) it)
                        (-map-indexed 'lsp-ui-peek--make-line it)
                        (-concat it (lsp-ui-peek--make-footer)))))
        (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
        (posframe-show lsp-ui-peek--buffer
                       :string (mapconcat 'identity string "")
                       :min-width (frame-width)
                       :internal-border-color (face-background 'posframe-border nil t)
                       :internal-border-width 1
                       :poshandler #'posframe-poshandler-frame-center))
      (funcall fn src1 src2))
    (defun lsp-ui-peek--peek-destroy (fn)
      (progn
        (when (bufferp lsp-ui-peek--buffer)
          (posframe-hide lsp-ui-peek--buffer))
        (setq lsp-ui-peek--last-xref nil))
      (funcall fn))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.7)))
              (propertize "\n" 'face '(:height 0.7))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.7)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

(use-package flymake
  :disabled
  :init (setq flymake-no-changes-timeout nil)
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook (prog-mode . flymake-mode))

(use-package sideline-flymake
  :disabled
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-fringe))

(use-package vterm
  :ensure-system-package cmake
  :custom
  (vterm-timer-delay 0.05)
  :config
  (defun pt/turn-off-chrome ()
    (hl-line-mode -1)
    ;;(yascroll-bar-mode nil)
    (display-line-numbers-mode -1))

  (defun pt/project-run-vterm ()
    "Invoke `vterm' in the project's root.

 Switch to the project specific term buffer if it already exists."
    (interactive)
    (let* ((project (project-current))
           (buffer (format "*vterm %s*" (consult--project-name (project-root project)))))
      (unless (buffer-live-p (get-buffer buffer))
        (unless (require 'vterm nil 'noerror)
          (error "Package 'vterm' is not available"))
        (vterm buffer)
        (vterm-send-string (concat "cd " (project-root project)))
        (vterm-send-return))
      (switch-to-buffer buffer)))

  :hook (vterm-mode . pt/turn-off-chrome))

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project)
  :bind (("C-c t" . #'vterm-toggle)
         :map vterm-mode-map
         ("C-\\" . #'popper-cycle)
         ("s-t" . #'vterm) ; Open up new tabs quickly
         ("s-v" . #'vterm-yank)
         ("C-y" . #'vterm-yank)
         ("C-h" . #'vterm-send-backspace)))

(use-package prodigy
  :bind (("C-c 8" . #'prodigy)
         :map prodigy-view-mode-map
         ("$" . #'end-of-buffer))
  :custom (prodigy-view-truncate-by-default t)
  :config
  (load "~/.config/emacs/services.el" 'noerror))

(use-package yasnippet-snippets)

(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  (setq yas-snippet-dirs
        '("/Users/afu/workspace/snippets"))
  :custom (yas-prompt-functions '(yas-completing-prompt)))

(use-package doom-snippets
  :load-path "/Users/afu/workspace/snippets"
  :after yasnippet)

;; (use-package rust-mode
;;   :defer t
;;   :custom
;;   (rust-format-on-save t)
;;   (lsp-rust-server 'rust-analyzer))

(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c a t" . rustic-cargo-current-test)
              ("C-c m" . rustic-compile))
  :custom
  (rustic-lsp-setup-p nil)
  (rustic-lsp-client 'lsp-mode)
  (rustic-format-on-save t))

;; after rustic and flycheck
(use-package flycheck-rust
  :after rustic-mode
  :custom
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package typescript-mode
  :custom (typescript-indent-level 2))
(use-package csharp-mode :defer t)
(setq-default js-indent-level 2)

(use-package dyalog-mode :defer t)

(use-package js2-mode
  :hook (js2-mode . js2-imenu-extras-mode)
  :mode ("\\.js$" . js2-mode)
  :ensure t
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t)
  )

(use-package xref-js2
  :ensure t
  :hook (js2-mode . pt/js-hook)
  :custom
  (xref-js2-search-program 'rg)
  :config
  (defun pt/js-hook ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package yaml-mode :defer t)
(use-package toml-mode :defer t)

(use-package protobuf-mode :defer t)

(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package web-mode
  :custom (web-mode-markup-indent-offset 2)
  :mode ("\\.html.erb$" . web-mode)
  :mode ("\\.art$" . web-mode))

(use-package typo :defer t)

(setq sh-basic-offset 2
      sh-basic-indentation 2)

(use-package google-this
  :bind ("C-c G" . #'google-this))

(use-package makefile-executor
  :bind ("C-c M" . makefile-executor-execute-project-target))

(use-package just-mode)

(use-package restclient
  :mode ("\\.restclient$" . restclient-mode))

(require 'tramp)
(setq remote-file-name-inhibit-locks t)

;; Needs to be called from recentf's :init
;; todo: make this into a use-package invocation
(defun pt/customize-tramp ()

  (setq tramp-default-method "ssh"
        tramp-verbose 1
        remote-file-name-inhibit-cache nil
        tramp-use-ssh-controlmaster-options nil
        tramp-default-remote-shell "/bin/bash"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch . "-c")))

  (connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
                                          '((shell-file-name . "/bin/bash")
                                            (shell-command-switch . "-c"))))

(use-package recentf
  :pin gnu
  :after dash
  :init (pt/customize-tramp) ;; so that tramp urls work ok in recentf
  :custom
  ;; (recentf-exclude (-concat recentf-exclude '("\\elpa"
  ;;                                             "private/tmp" ; to avoid custom files
  ;;                                             "txt/roam"
  ;;                                             "type-break"
  ;;                                             )))
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 30)
  :config (recentf-mode))

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))
