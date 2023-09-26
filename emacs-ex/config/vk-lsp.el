;;; vk-magit.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Built-in xref and eldoc are powerful packages
(use-package xref
  :pin gnu :ensure t
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("s-r" . #'xref-find-references)
         ("C-<down-mouse-1>" . #'xref-find-definitions)
         ("C-S-<down-mouse-1>" . #'xref-find-references)
         ("C-<down-mouse-2>" . #'xref-go-back)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t))

;; eglot
(use-package eglot
  :hook ((go-mode . eglot-ensure)
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
  (defvar pt/disable-haskell-here t)
  (defun pt/haskell-eglot-except-tidal ()
    (unless (or pt/disable-haskell-here
                (string-equal "tidal"
                              (file-name-extension (buffer-file-name))))
      (eglot-ensure)))
  ;; Eglot doesn't correctly unescape markdown:
  ;; https://github.com/joaotavora/eglot/issues/333
  (defun mpolden/gfm-unescape-string (string)
    "Remove backslash-escape of punctuation characters in STRING."
    ;; https://github.github.com/gfm/#backslash-escapes
    (replace-regexp-in-string
     "[\\\\]\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)" "\\1"
     string))

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

;; built-in flymake does a great job, and eglot builds upon it.
(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)))


(provide 'vk-lsp)
;;; vk-lsp.el ends here
