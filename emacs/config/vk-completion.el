;; vk-completion.el -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; nicer and faster than Ivy.
(use-package vertico
  :ensure t
  :demand
  :config
  (vertico-mode t)
  (vertico-mouse-mode)
  (set-face-attribute 'vertico-mouse nil :inherit nil)
  (savehist-mode)
  :custom
  (vertico-count 22)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-'"           . vertico-quick-exit)
              ("C-c '"         . vertico-quick-insert)
              ("<return>"      . exit-minibuffer)
              ("C-m"           . vertico-insert)
              ("C-c SPC"       . vertico-quick-exit)
              ("C-<backspace>" . vertico)
              ("DEL"           . vertico-directory-delete-char)))

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

(use-package marginalia
  :config (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic)))

(use-package ctrlf
  :config (ctrlf-mode))

(use-package prescient
  :config (prescient-persist-mode))

;; It is pretty good at figuring out where declarations of things might be.
(use-package dumb-jump
  :config
  (defun pt/quietly-dumb-jump ()
    (interactive)
    (shut-up (call-interactively 'dumb-jump-go)))
  :bind (("C-c J" . #'pt/quietly-dumb-jump)))

;; embark is a cool package for discoverability.
(use-package embark
  :bind ("C-c e" . #'embark-act)
  :bind ("C-<escape>" . #'embark-act))

(use-package embark-consult :after (embark consult))
(use-package embark-vc :after embark)

(provide 'vk-completion)
;;; vk-completion.el ends here
