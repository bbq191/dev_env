;; vk-minibuffer.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; embark is a cool package for discoverability.
(use-package embark
  :after vertico
  :bind (:map vertico-map
              ("C-c e" . embark-export)
              ("C-<escape>" . embark-act)))
(use-package consult
  :bind* (("C-c r"     . consult-recent-file))
  :bind (("C-c i"     . consult-imenu)
         ("C-c b"     . consult-project-buffer)
         ("C-x b"     . consult-buffer)
         ("C-c B"     . consult-bookmark)
         ("C-c `"     . flymake-goto-next-error)
         ("C-c h"     . consult-ripgrep)
         ("C-c y"     . consult-yank-pop)
         ("C-x C-f"   . find-file)
         ("C-c C-h a" . describe-symbol))
  :custom
  (consult-narrow-key (kbd ";"))
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
  (consult-preview-key '(:debounce 0.25 any)))

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
