;; vk-completion.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; nicer and faster than Ivy.
(use-package vertico
  :ensure t
  :demand
  :config
  (vertico-mode t)
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

(use-package marginalia
  :config (marginalia-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (setq orderless-style-dispatchers '(+orderless-dispatch)
         orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         ;; (global-corfu-mode . corfu-popupinfo-mode)
         (lsp-completion-mode . kb/corfu-setup-lsp)) ; Use corfu for lsp completion
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :general (:keymaps 'corfu-map ;; :states 'insert is not necessory
                     "C-n" #'corfu-next
                     "C-p" #'corfu-previous
                     "<escape>" #'corfu-quit
                     "<return>" #'corfu-insert
                     "M-d" #'corfu-popupinfo-toggle)
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 1)

  (corfu-min-width 30)
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

;; completion extension
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

(use-package ctrlf
  :config (ctrlf-mode))

(use-package prescient
  :config (prescient-persist-mode))

;; It is pretty good at figuring out where declarations of things might be.
(use-package dumb-jump
  :config
  (defun pt/quietly-dumb-jump ()
    (interactive)
    (shut-up (call-interactively 'dumb-jump-go)))
  :bind (("C-c J" . #'pt/quietly-dumb-jump)))

;; embark is a cool package for discoverability.
(use-package embark
  :bind ("C-c e" . #'embark-act)
  :bind ("C-<escape>" . #'embark-act))

(use-package embark-consult :after (embark consult))
(use-package embark-vc :after embark)

;; just using the builtin completion-at-point facilities for autocomplete
(bind-key* "C-." #'completion-at-point)


(provide 'vk-completion)
;;; vk-completion.el ends here
