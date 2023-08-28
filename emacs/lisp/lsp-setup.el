;; init-base.el --- LSP configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; lsp and eglot.
;;
;;; Code:

;; lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((prog-mode . (lambda ()
;;                         (unless (derived-mode-p 'emacs-lisp-mode 
;;                         'lisp-mode
;;                         'markdown-mode
;;                         'makefile-mode 
;;                         'snippet-mode)
;;                           (lsp-deferred))))
;;          ((yaml-mode yaml-ts-mode) . lsp-deferred))
;;   :bind (:map lsp-mode-map
;;               ("C-c f" . lsp-format-region)
;;               ("C-c d" . lsp-describe-thing-at-point)
;;               ("C-c a" . lsp-execute-code-action)
;;               ("C-c r" . lsp-rename))
;;   ;;:config (with-no-warnings
;;   ;;        (lsp-enable-which-key-integration t))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-enable-links nil)                    ;; no clickable links
;;   (lsp-enable-folding nil)                  ;; use `hideshow' instead
;;   (lsp-enable-snippet t)                    ;; no snippets, it requires `yasnippet'
;;   (lsp-enable-file-watchers nil)            ;; performance matters
;;   (lsp-enable-text-document-color nil)      ;; as above
;;   (lsp-enable-symbol-highlighting t)        ;; as above
;;   (lsp-enable-on-type-formatting nil)       ;; as above
;;   (lsp-semantic-tokens-enable nil)          ;; optional
;;   (lsp-semantic-tokens-apply-modifiers nil) ;; don't override token faces
;;   (lsp-headerline-breadcrumb-enable nil)    ;; keep headline clean
;;   (lsp-modeline-code-actions-enable nil)    ;; keep modeline clean
;;   (lsp-modeline-diagnostics-enable t)       ;; as above
;;   (lsp-log-io nil)                          ;; debug only
;;   (lsp-auto-guess-root t)                   ;; Yes, I'm using projectile
;;   (lsp-completion-provider :none)           ;; don't add `company-capf' to `company-backends'
;;   (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
;;   (lsp-eldoc-enable-hover nil)              ;; disable eldoc hover
;;   ;; 指定 flycheck 使用 clippy
;;   ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints nil)
;;   (lsp-rust-analyzer-display-reborrow-hints nil)

;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-keep-workspace-alive nil
              lsp-signature-auto-activate nil
              lsp-modeline-code-actions-enable t
              lsp-modeline-diagnostics-enable t
              lsp-modeline-workspace-status-enable t

              lsp-semantic-tokens-enable t
              lsp-progress-spinner-type 'progress-bar-filled

              lsp-enable-file-watchers nil
              lsp-enable-folding nil
              lsp-enable-symbol-highlighting nil
              lsp-enable-text-document-color nil

              lsp-enable-indentation nil
              lsp-enable-on-type-formatting nil

              ;; For diagnostics
              lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

              ;; For clients
              lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :config
  (use-package consult-lsp
    :bind (:map lsp-mode-map
                ("C-M-." . consult-lsp-symbols)))

  (with-no-warnings
    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (memq major-mode '(sh-mode bash-ts-mode))
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

    ;; Display icons
    (defun my-lsp-icons-get-symbol-kind (fn &rest args)
      (and (apply fn args)))
    (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

    ;; For `lsp-headerline'
    (defun my-lsp-icons-get-by-file-ext (fn &rest args)
      (and (apply fn args)))
    (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

    (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
      (when (and file-ext
                 (lsp-icons--enabled-for-feature feature))
        (nerd-icons-icon-for-extension file-ext)))
    (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

    (defvar lsp-symbol-alist
      '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
        (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
        (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
        (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
        (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
        (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
        (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
        (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
        (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
        (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
        (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
        (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
        (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
        (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
        (enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
        (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
        (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
        (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
        (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
        (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

    (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                   :face 'lsp-headerline-breadcrumb-separator-face))))




;; LSP Ui
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :custom (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover nil)
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-doc-enable nil))


(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("s-<return>" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 0.1
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
      (if (childframe-workable-p)
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
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
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
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))


;; Eglot - disabled
(use-package eglot
  :disabled
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c f" . eglot-format)
              ("C-c d" . eldoc-doc-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
              ("C-c l" . eglot-command-map))
  :config
  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    ;; workspaces
    "w q" #'eglot-shutdown
    "w r" #'eglot-reconnect
    "w s" #'eglot
    "w d" #'eglot-show-workspace-configuration

    ;; formatting
    "= =" #'eglot-format-buffer
    "= r" #'eglot-format

    ;; goto
    "g a" #'xref-find-apropos
    "g d" #'eglot-find-declaration
    "g g" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g t" #'eglot-find-typeDefinition

    ;; actions
    "a q" #'eglot-code-action-quickfix
    "a r" #'eglot-code-action-rewrite
    "a i" #'eglot-code-action-inline
    "a e" #'eglot-code-action-extract
    "a o" #'eglot-code-action-organize-imports)
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider)))



(provide 'lsp-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp-setup.el ends here
