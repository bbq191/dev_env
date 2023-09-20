;; vk-elisp.el --- vk-elisp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; Using paredit for all lisp file
(use-package paredit
  :after paredit
  :diminish (paredit-mode " Par")
  :hook ((paredit-mode-hook . sanityinc/maybe-map-paredit-newline)
         ;; Use paredit in the minibuffer
         ;; TODO: break out into separate package
         ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
         (minibuffer-setup-hook . sanityinc/conditionally-enable-paredit-mode)
         (sanityinc/lispy-modes-hook . enable-paredit-mode))
  :config
  (defun sanityinc/maybe-map-paredit-newline()
    (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))
  (defvar paredit-minibuffer-commands '(eval-expression
                                        pp-eval-expression
                                        eval-expression-with-eldoc
                                        ibuffer-do-eval
                                        ibuffer-do-view-and-eval)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  (defun sanityinc/conditionally-enable-paredit-mode()
    "Enable paredit during lisp-related minibuffer commands."
    (when (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("RET" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp-killing-backward))

(defun sanityinc/enable-check-parens-on-save()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(enable-paredit-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

  (defun sanityinc/lisp-setup()
    "Enable features useful in any Lisp mode."
    (run-hooks 'sanityinc/lispy-modes-hook))

(use-package derived
  :ensure nil
  :config
  (dolist (mode '(emacs-lisp-mode ielm-mode lisp-mode inferior-lisp-mode lisp-interaction-mode))
    (add-hook (derived-mode-hook-name mode) 'sanityinc/lisp-setup))

  (when (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

  (add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode)))

(provide 'vk-elisp)

;;; vk-elisp.el ends here
