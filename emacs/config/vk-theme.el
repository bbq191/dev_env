;; vk-theme.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
      (if (and (project-current) (not org-src-mode) project-relative)
          (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
        (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'pt/mood-line-segment-project-advice)
  (mood-line-mode))


(provide 'vk-theme)
;;; vk-theme.el ends here
