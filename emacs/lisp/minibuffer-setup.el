;; init-base.el --- Minibuffer configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;;; Code:

;; Marginalia - Marginalia is painless to set up
(use-package marginalia
  :ensure t
  :general (:keymaps 'minibuffer-local-map "M-A" 'marginalia-cycle)
  :custom (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init (marginalia-mode))


;; Embark with Consult
;; embark
(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map ("M-o"     . embark-act)
              ("C-c C-c" . embark-export)
              ("C-c C-o" . embark-collect)))


;; consult
(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store))
  :config (with-no-warnings
            (consult-customize consult-ripgrep 
                               consult-git-grep 
                               consult-grep
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
  :custom (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))


;; Consult users will also want the embark-consult package.
(use-package embark-consult :ensure t :after embark consult)


;; Vertico is a minibuffer interface, that is, it changes the minibuffer looks
;; and how you interact with it.
(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :elpaca (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
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



;; Orderless is an alternative and powerful completion style, that is, it is an alternative to Emacs’s basic candidate-filtering capacities.
(use-package orderless
  :ensure t
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



(provide 'minibuffer-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minibuffer-setup.el ends here
