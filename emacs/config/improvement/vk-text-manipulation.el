;; vk-text-manipulation.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Any modern editor should include multiple-cursor support.;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :bind (("C-c C-e m" . #'mc/edit-lines)
         ("C-c C-e d" . #'mc/mark-all-dwim)))

;; Textmate-style tap-to-expand-into-the-current-delimiter is very useful and
;; curiously absent.
(use-package expand-region
  :bind (("C-c n" . er/expand-region)))
(bind-key* "C-c /" #'comment-dwim)
(bind-key* "C-c 0" #'capitalize-dwim)

;; avy gives us fluent jump-to-line commands mapped to the home row.
(use-package avy
  :bind (("C-c j k" . #'avy-kill-whole-line)
         ("C-c j h" . #'avy-kill-region)
         ("C-c j w" . #'avy-copy-line)
         ("C-c v" . #'avy-goto-char)))

(use-package avy-zap
  :bind (("C-c z" . #'avy-zap-to-char)
         ("C-c Z" . #'avy-zap-up-to-char)))

;; iedit gives us the very popular idiom of automatically deploying multiple
;; cursors to edit all occurrences of a particular word.
(shut-up (use-package iedit
           :bind (:map iedit-mode-keymap ("C-h" . #'sp-backward-delete-char))
           :bind ("C-;" . #'iedit-mode)))

;; provides a better editing experience.
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

;; provides a shortcut (C-c C--) to insert the prefix.
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

(provide 'vk-text-manipulation)
;;; vk-text-manipulation.el ends here
