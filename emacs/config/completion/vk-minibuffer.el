;; vk-minibuffer.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; embark is a cool package for discoverability.
(use-package embark
  :after vertico
  :general (:keymaps 'vertico-map
                     "C-c e" #'embark-export
                     "C-<escape>" #'embark-act))

(use-package consult
  :custom
  (defmacro sanityinc/no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key "M-P")))

  (sanityinc/no-consult-preview
   consult-ripgrep
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

  (when (and (executable-find "rg"))
    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                      (symbol-name s))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

(use-package embark-vc :after embark)
(use-package consult-flycheck)

(use-package marginalia
  :hook (after-init . marginalia-mode))

;; just using the builtin completion-at-point facilities for autocomplete
(bind-key* "C-." #'completion-at-point)

(provide 'vk-minibuffer)

;;; vk-minibuffer.el ends here
