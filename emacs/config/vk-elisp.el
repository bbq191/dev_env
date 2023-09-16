;; vk-elisp.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Enable desired features for all lisp modes
(defun vk/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar vk/lispy-modes-hook
  '(enable-paredit-mode
    vk/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")


(when (use-package aggressive-indent)
  (add-to-list 'vk/lispy-modes-hook 'aggressive-indent-mode))

(defun vk/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'vk/lispy-modes-hook))

(require 'derived)

(dolist (mode '(emacs-lisp-mode ielm-mode lisp-mode inferior-lisp-mode lisp-interaction-mode))
  (add-hook (derived-mode-hook-name mode) 'vk/lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

;; Paredit
(use-package paredit)


(defun vk/maybe-map-paredit-newline ()
  (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'vk/maybe-map-paredit-newline)

(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par")
  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("RET" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp-killing-backward))



;; Use paredit in the minibuffer
;; TODO: break out into separate package
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(add-hook 'minibuffer-setup-hook 'vk/conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun vk/conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (when (memq this-command paredit-minibuffer-commands)
    (enable-paredit-mode)))

(add-hook 'vk/lispy-modes-hook 'enable-paredit-mode)

(provide 'vk-elisp)

;;; vk-elisp.el ends here
