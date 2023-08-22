;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(defconst vk/macsys (eq system-type 'darwin))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(defun ikate/adjust-opacity (frame incr)
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

(defun fix-fullscreen ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and vk/macsys
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      ring-bell-function 'ignore        ; No annoying bell
      blink-cursor-mode nil             ; No eyes distraction
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

(desktop-save-mode 1)
(save-place-mode 1)
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
;; recentf setting
(recentf-mode 1)
(setq recentf-max-saved-items 300)
;; basic ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil) ;; 关闭使用系统自带的文件选择对话框,使用Emacs自带的。
(setq use-dialog-box nil) ;; 关闭使用系统自带的消息框,使用Emacs自带的。
(setq inhibit-startup-screen t) ;; 阻止显示启动画面。
(setq inhibit-startup-message t) ;; 阻止显示启动消息。
(setq inhibit-startup-buffer-menu t) ;; 阻止显示启动缓冲区菜单。
(setq window-resize-pixelwise t) ;; 设置窗口调整大小时以像素为单位。
(setq frame-resize-pixelwise t);; 设置框架调整大小时以像素为单位。
;; line number
(global-display-line-numbers-mode 1)
;; (global-visual-line-mode t)
;; (setq display-line-numbers-type 'relative)
(setq-default bidi-paragraph-direction 'left-to-right) ; 修改双向文字排版为从左到右
(setq bidi-inhibit-bpa t)

(with-no-warnings
  ;; Key Modifiers
  (setq mac-option-modifier 'meta
    mac-command-modifier 'super)
  (bind-keys ([(super a)] . mark-whole-buffer)
         ([(super c)] . kill-ring-save)
         ([(super l)] . goto-line)
         ([(super q)] . save-buffers-kill-emacs)
         ([(super s)] . save-buffer)
         ([(super v)] . yank)
         ([(super w)] . delete-frame)
         ([(super z)] . undo)))
;; reload init
(bind-keys ("C-s-r" . reload-init-file))
;; recent file
(bind-keys ("C-x C-r" . recentf-open-files))
;; zooming in/out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; 调整界面 opacity
(global-set-key (kbd "M-C-8") (lambda () (interactive) (ikate/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (ikate/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(use-package diminish)

(use-package general)

(use-package no-littering)

(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package company)
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(set-face-attribute 'default nil
    :font "Cascadia Code"
    :height 150
    :weight 'regular)
(set-face-attribute 'variable-pitch nil
    :font "Symbols Nerd Font"
    :height 150
    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
    :font "FiraCode Nerd Font"
    :height 150
    :weight 'regular)

(set-face-attribute 'font-lock-keyword-face nil
    :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Cascadia Code-15"))
(setq-default line-spacing 0.12)

(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen)
  (and vk/macsys (bind-key "C-s-f" #'toggle-frame-fullscreen)))

(use-package doom-themes
:config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; Sets the default theme to load!!! 
(load-theme 'doom-one t)
;; Enable custom neotree theme (all-the-icons must be installed!)
;; s(doom-themes-neotree-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config))

(use-package doom-modeline
:ensure t
:init (doom-modeline-mode 1)
:config
(setq doom-modeline-height 35      ;; sets modeline height
      doom-modeline-bar-width 5    ;; sets right bar width
      doom-modeline-persp-name t   ;; adds perspective name to modeline
      doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :general
  (:keymaps '(normal insert visual motion)
   "M-." #'vertico-repeat)
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
   "<escape>" #'minibuffer-keyboard-quit
   "?" #'minibuffer-completion-help
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   ;; Multiform toggles
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter
   "C-i" #'vertico-quick-insert
   "C-o" #'vertico-quick-exit
   "M-o" #'kb/vertico-quick-embark
   "M-G" #'vertico-multiform-grid
   "M-F" #'vertico-multiform-flat
   "M-R" #'vertico-multiform-reverse
   "M-U" #'vertico-multiform-unobtrusive
   "C-l" #'kb/vertico-multiform-flat-toggle)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save)) ; Make sure vertico state is saved
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand))))

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect)))

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
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

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :hook ((lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
         (kb/corfu-setup-lsp . corfu-popupinfo-mode))
  :general
  (:keymaps 'corfu-map
   :states 'insert
   "C-n" #'corfu-next
   "C-p" #'corfu-previous
   "<escape>" #'corfu-quit
   "<return>" #'corfu-insert
   "M-s-SPC" #'corfu-insert-separator
   ;; "SPC" #'corfu-insert-separator ; Use when `corfu-quit-at-boundary' is non-nil
   "M-d" #'corfu-show-documentation
   "C-g" #'corfu-quit
   "M-l" #'corfu-show-location)
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
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
  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (global-corfu-mode)
  :config
  ;; NOTE 2022-03-01: This allows for a more evil-esque way to have
  ;; `corfu-insert-separator' work with space in insert mode without resorting to
  ;; overriding keybindings with `general-override-mode-map'. See
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  ;; Alternatively, add advice without `general.el':
  ;; (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  ;; (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  ;; (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
  ;; (evil-make-overriding-map corfu-map)

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

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package cape
  :hook ((emacs-lisp-mode .  kb/cape-capf-setup-elisp)
         (lsp-completion-mode . kb/cape-capf-setup-lsp)
         (org-mode . kb/cape-capf-setup-org)
         (eshell-mode . kb/cape-capf-setup-eshell)
         (git-commit-mode . kb/cape-capf-setup-git-commit)
         (sh-mode . kb/cape-capf-setup-sh))
  :general (:prefix "M-p"               ; Particular completion function
                    "p" 'completion-at-point
                    "t" 'complete-tag           ; etags
                    "d" 'cape-dabbrev           ; or dabbrev-completion
                    "f" 'cape-file
                    "k" 'cape-keyword
                    "s" 'cape-lisp-symbol
                    "a" 'cape-abbrev
                    "i" 'cape-ispell
                    "l" 'cape-line
                    "w" 'cape-dict
                    "\\"'cape-tex
                    "_" 'cape-tex
                    "^" 'cape-tex
                    "&" 'cape-sgml
                    "r" 'cape-rfc1345)
  :custom (cape-dabbrev-min-length 3)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Elisp
  (defun kb/cape-capf-ignore-keywords-elisp (cand)
    "Ignore keywords with forms that begin with \":\" (e.g.
:history)."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-lisp-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file)
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Org
  (defun kb/cape-capf-setup-org ()
    (require 'org-roam)
    (if (org-roam-file-p)
        (org-roam--register-completion-functions-h)
      (let (result)
        (dolist (element (list
                          (cape-capf-super #'cape-ispell #'cape-dabbrev)
                          (cape-company-to-capf #'company-yasnippet))
                         result)
          (add-to-list 'completion-at-point-functions element)))))

  ;; Eshell
  (defun kb/cape-capf-setup-eshell ()
    (let ((result))
      (dolist (element '(pcomplete-completions-at-point cape-file) result)
        (add-to-list 'completion-at-point-functions element))))

  ;; Git-commit
  (defun kb/cape-capf-setup-git-commit ()
    (general-define-key
     :keymaps 'local
     :states 'insert
     "<tab>" 'completion-at-point)      ; Keybinding for `completion-at-point'
    (let ((result))
      (dolist (element '(cape-dabbrev cape-symbol) result)
        (add-to-list 'completion-at-point-functions element))))

  ;; Sh
  (defun kb/cape-capf-setup-sh ()
    (require 'company-shell)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
    :config
    ;; For pcomplete. For now these two advices are strongly recommended to
    ;; achieve a sane Eshell experience. See
    ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
    ;; `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(require 'org-tempo)

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-region)
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action)
              ("C-c r" . lsp-rename))
  ;; :config
  ;; (with-no-warnings
  ;; (lsp-enable-which-key-integration t))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-links nil)                    ;; no clickable links
  (lsp-enable-folding nil)                  ;; use `hideshow' instead
  (lsp-enable-snippet nil)                  ;; no snippets, it requires `yasnippet'
  (lsp-enable-file-watchers nil)            ;; performance matters
  (lsp-enable-text-document-color nil)      ;; as above
  (lsp-enable-symbol-highlighting t)        ;; as above
  (lsp-enable-on-type-formatting nil)       ;; as above
  (lsp-semantic-tokens-enable nil)          ;; optional
  (lsp-semantic-tokens-apply-modifiers nil) ;; don't override token faces
  (lsp-headerline-breadcrumb-enable nil)    ;; keep headline clean
  (lsp-modeline-code-actions-enable nil)    ;; keep modeline clean
  (lsp-modeline-diagnostics-enable t)       ;; as above
  (lsp-log-io nil)                          ;; debug only
  (lsp-auto-guess-root t)                   ;; Yes, I'm using projectile
  (lsp-completion-provider :none)           ;; don't add `company-capf' to `company-backends'
  (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)              ;; disable eldoc hover
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)


  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-analyzer-command '("~/.local/share/cargo/bin/rust-analyzer"))
  (setq rustic-flycheck-clippy-params "--message-format=json -Zunstable-options")
  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'kv/rustic-mode-hook))

(defun kv/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package rust-playground :ensure)

(use-package toml-mode :ensure)



;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

;; inline
(use-package flycheck-inline)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
;; for rust
(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
