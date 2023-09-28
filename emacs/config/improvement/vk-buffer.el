;; vk-buffer.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; default to a two-buffer setup.-- using evil replace
;; (defun revert-to-two-windows ()
;;  "Delete all other windows and split it into two."
;;  (interactive)
;;  (delete-other-windows)
;;  (split-window-right))

;;(bind-key "C-x 1" #'revert-to-two-windows)
;;(bind-key "C-x !" #'delete-other-windows) ;; Access to the old keybinding.

;; if the minibuffer is open, so here’s a beefed-up version.
(defun pt/abort ()
  "Remove auxiliary buffers."
  (interactive)
  (ignore-errors (exit-recursive-edit))
  (ignore-errors (ctrlf-cancel))
  (popper-close-latest)
  (call-interactively #'keyboard-quit))

(bind-key* "s-g" #'pt/abort)

;; copy a filename to the clipboard
(defun copy-file-name-to-clipboard (do-not-strip-prefix)
  "Copy the current buffer file name to the clipboard. The path will be relative to the project's root directory, if set. Invoking with a prefix argument copies the full path."
  (interactive "P")
  (let
      ((filename (pt/project-relative-file-name do-not-strip-prefix)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(bind-key "C-c p" #'copy-file-name-to-clipboard)

;; switch to scratch
(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(bind-key "C-c a s" #'switch-to-scratch-buffer)

;; One of the main problems with Emacs is how many ephemeral buffers it creates.
;; I’m giving popper-mode a try to see if it can stem the flood thereof.
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
                              compilation-mode
                              bqn-inferior-mode)))


(provide 'vk-buffer)
;;; vk-buffer.el ends here