;; vk-improvement.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; One of Emacs’s most broken UI decisions is to prompt for saving buffers that
;; are marked as modified, even if their contents are the same as on disc.
(defun pt/check-file-modification (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p)
                 (not (file-remote-p buffer-file-name))
                 (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (autoload 'diff-no-select "diff")
  (when buffer-file-name
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

(advice-add 'magit-status :before #'pt/check-file-modification)
(advice-add 'save-buffers-kill-terminal :before #'pt/check-file-modification)

;; edit a file as root
(use-package sudo-edit)

;; Dired needs a couple customizations to work in a sensible manner.
(setq dired-use-ls-dired nil ;; I use exa, which doesn't have a --dired flag
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

(use-package dired-preview
  :disabled
  :after dired
  :hook (dired-mode . dired-preview-mode))

;; duplicate whatever’s marked
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

;; we can automatically chmod a file containing a shebang into executable mode.
(setq executable-prefix-env t)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(context-menu-mode)
(bind-key "C-c C-m" #'tmm-menubar)

(provide 'vk-improvement)
;;; vk-improvement.el ends here
