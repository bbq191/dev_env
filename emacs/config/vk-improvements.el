;; vk-text-manipulation.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pt/split-window-thirds ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(bind-key "C-c 3" #'pt/split-window-thirds)

;; Given how often I tweak my config, I bind C-c E to take me to my config file.
(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(bind-key "C-c E" #'open-init-file)

;; It’s weird that Emacs doesn’t come with a standard way to insert the current
;; date.
(defun pt/insert-current-date ()
  "Insert the current date (Y-m-d) at point."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(bind-key "s-w" #'kill-this-buffer)

;; One of Emacs’s most broken UI decisions is to prompt for saving buffers that
;; are marked as modified, even if their contents are the same as on disc.
(defun pt/check-file-modification (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (not (file-remote-p buffer-file-name)) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (autoload 'diff-no-select "diff")
  (when buffer-file-name
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

;; (advice-add 'save-some-buffers :before #'pt/check-file-modification)

;; (add-hook 'before-save-hook #'pt/check-file-modification)
;; (add-hook 'kill-buffer-hook #'pt/check-file-modification)
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
(global-so-long-mode)

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

;; incrementing and decrementing numbers
(use-package evil-numbers
  :bind ("C-c a 1" . #'evil-numbers/inc-at-pt))

(use-package which-key
  :diminish
  :custom
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; we can automatically chmod a file containing a shebang into executable mode.
(setq executable-prefix-env t)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(context-menu-mode)
(bind-key "C-c C-m" #'tmm-menubar)

(provide 'vk-improvements)
;;; vk-improvements.el ends here
