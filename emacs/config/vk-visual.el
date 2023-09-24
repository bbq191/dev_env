;; vk-visual.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Why Emacs doesn’t allow colors by default in its compilation buffer
(use-package fancy-compilation :config (fancy-compilation-mode))

;; Icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Nerd-Symbol-Mono is necessory
(use-package nerd-icons :demand t)

;; For dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; For completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; For treemacs
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; For ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; theme and mode lien ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :demand t
  :config
  (let ((chosen-theme 'doom-wilmersdorf))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-rouge-brighter-comments t
          doom-ir-black-brighter-comments t
          modus-themes-org-blocks 'gray-background
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme t)))

;;With a bit of elbow grease, it can be convinced to show the project-relative file name.
(defun pt/project-relative-file-name (include-prefix)
  "Return the project-relative filename, or the full path if INCLUDE-PREFIX is t."
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (if fullname (file-relative-name fullname root) fullname))
       (should-strip (and root (not include-prefix))))
    (if should-strip relname fullname)))

(use-package mood-line
  :config
  (defun pt/mood-line-segment-project-advice (oldfun)
    "Advice to use project-relative file names where possible."
    (let ((project-relative (ignore-errors (pt/project-relative-file-name nil))))
      (if (and (project-current) project-relative)
          (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
        (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'pt/mood-line-segment-project-advice)
  (mood-line-mode))

;; I may be colorblind, but it’s good enough, usually. ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :disabled
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; given the considerable size of my screen. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package centered-window
  :custom
  (cwm-centered-window-width 180))

;; Compilation buffers should wrap their lines.
(add-hook 'compilation-mode-hook 'visual-line-mode)

;; URLs should be highlighted and linkified.
(global-goto-address-mode)

(provide 'vk-visual)
;;; vk-visual.el ends here
