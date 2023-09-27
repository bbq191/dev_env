;; vk-org-mode.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I don’t take advantage of org-agenda, org-timer, org-calendar, org-capture, anything interesting to do with tags, et cetera. Someday I will learn these things, but not yet.
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
  (org-src-window-setup 'current-window)
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

(use-package ob-mermaid)

(provide 'vk-org-mode)
;;; vk-org-mode.el ends here