;; vk-keybind.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; change key for mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Global keybind
(use-package general
  :config
  (general-create-definer vk-leader-key :prefix "C-c")
  ;; Help command
  (general-create-definer vk-help-key :prefix "C-h")
  ;; Query command
  (general-create-definer vk-search-key :prefix "C-;")
  ;; Execute command
  (general-create-definer vk-exec-key :prefix "C-x")
  
  
  (vk-exec-key
    "r" '((lambda () (interactive)
            (load-file "~/.config/emacs/init.el")
            (ignore (elpaca-process-queues)))
          :wk "Reload emacs config")
    "," '((lambda () (interactive)
            (dired "~/Workspace/dotfiles/emacs/"))
          :wk "Open user-emacs-directory in dired")
    ;; "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "/" '(comment-line :wk "Comment lines")
    "K" '(save-buffers-kill-emacs :wk "Kill emacs"))

  (vk-leader-key
    "b" '(:ignore t :wk "Buffers")
    "b b" '(consult-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(consult-buffer-other-window :wk "Switch to buffer other window")
    "b W" '(consult-buffer-other-frame :wk "Switch to buffer other frame"))
  
  (vk-leader-key
    ;;TODO move lsp key bind to here
    "c" '(:ignore t :wk "Code") ;; see some in lsp config file
    "e l" '(flycheck-list-errors :wk "List all errors")
    :keymaps 'lsp-mode-map
    "c f" '(lsp-format-region)
    "c d" '(lsp-describe-thing-at-point)
    "c A" '(lsp-execute-code-action)
    "c r" '(lsp-rename)
    "g D" '(lsp-find-definition)
    "g r" '(lsp-find-references))
  
  (vk-leader-key
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree"))

  (vk-leader-key
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (vk-leader-key
    "f" '(:ignore t :wk "Files")
    "f f" '(find-file :wk "Find file")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (vk-leader-key
    "g" '(:ignore t :wk "Goto")
    "g e" '(consult-compile-error :wk "Goto compile error")
    "g f" '(consult-flycheck :wk "Goto flycheck")
    "g g" '(consult-goto-line :wk "Goto line")
    "g o" '(consult-outline :wk "Goto outline")
    "g m" '(consult-mark :wk "Goto mark")
    "g k" '(consult-global-mark :wk "Goto global mark")
    "g i" '(consult-imenu :wk "Goto imenu")
    "g I" '(consult-imenu-multi :wk "Goto multi imenu"))
  
  (vk-help-key
    "C-h" '(:ignore t :wk "Help")
    ":" '(consult-complex-command :wk "Repeat complex command")
    "b" '(describe-bindings :wk "Describe bindings")
    "c" '(describe-char :wk "Describe character under cursor")
    "e" '(view-echo-area-messages :wk "View echo area messages")
    "f" '(describe-function :wk "Describe function")
    "F" '(describe-face :wk "Describe face")
    "g" '(describe-gnu-project :wk "Describe GNU Project")
    "I" '(describe-input-method :wk "Describe input method")
    "l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "L" '(describe-language-environment :wk "Describe language environment")
    "m" '(describe-mode :wk "Describe mode")
    "t" '(load-theme :wk "Load theme")
    "v" '(describe-variable :wk "Describe variable")
    "w" '(where-is :wk "Prints keybinding for command if set")
    "x" '(describe-command :wk "Display full documentation for command"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (vk-exec-key
    "p" '(:ignore :wk "Project"))

  (vk-leader-key
    "r" '(:ignore t :wk "Register")
    "r r" '(consult-register :wk "Consult register")
    "r s" '(consult-register-store :wk "Consult register store")
    "r l" '(consult-register-load :wk "Consult register load"))
  
  (vk-search-key
    "C-;" '(:ignore t :wk "Search")
    "f" '(consult-find :wk "Find")
    "F" '(consult-locate :wk "Locate a file")
    "g" '(consult-grep :wk "Grep")
    "G" '(consult-git-grep :wk "Git grep")
    "r" '(query-replace :wk "Query replace")
    "R" '(consult-ripgrep :wk "Ripgrep")
    "l" '(consult-line :wk "Line")
    "L" '(consult-line-multi :wk "Multi line")
    "m" '(consult-multi-occur :wk "Multi occur")
    "k" '(consult-keep-lines :wk "Keep lines")
    "u" '(consult-focus-lines :wk "Focus lines")
    "d" '(dictionary-search :wk "Search dictionary")
    "m" '(consult-man :wk "Man pages")
    "t" '(tldr :wk "Lookup TLDR docs for a command")
    "w" '(woman :wk "Similar to man but doesn't require man")))

;; Block until current queue processed.
(elpaca-wait)

(provide 'vk-keybind)

;;; vk-keybind.el ends here
