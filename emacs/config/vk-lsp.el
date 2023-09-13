;; vk-lsp.el --- vk-lsp configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; lsp-mode
(use-package lsp-mode
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred))
  :general (:keymaps 'lsp-mode-map :states 'normal
                     "SPC cf" #'lsp-format-region
                     "SPC cd" #'lsp-describe-thing-at-point
                     "SPC cA" #'lsp-execute-code-action
                     "SPC cr" #'lsp-rename
                     "g D" #'lsp-find-definition
                     "g r" #'lsp-find-references)
  
  :init (setq lsp-keymap-prefix ""
              lsp-keep-workspace-alive nil
              lsp-signature-auto-activate nil
              lsp-modeline-code-actions-enable nil
              lsp-modeline-diagnostics-enable nil
              lsp-modeline-workspace-status-enable nil
              ;; For doc
              lsp-ui-doc-enable t
              lsp-signature-auto-activate t

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
    :general (:keymaps 'lsp-mode-map :states 'normal
                       "SPC ls" #'consult-lsp-symbols))
  ;; (with-no-warnings
  ;; (lsp-enable-which-key-integration t))

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

    (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                   :face 'lsp-headerline-breadcrumb-separator-face))))

(provide 'vk-lsp)

;;; vk-lsp.el ends here
