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
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer vk/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; Help command
  (general-create-definer vk-help-key :prefix "C-h")

  ;; For command remap
  (general-define-key
   [remap query-replace-regexp] 'anzu-query-replace-regexp
   [remap query-replace] 'anzu-query-replace
   [remap switch-to-buffer] 'consult-buffer
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
   [remap goto-line] 'consult-goto-line)

  (vk/leader-keys
    "" '(:ignore t :wk "System")
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "Find file")
    ;; "=" '(perspective-map :wk "Perspective")
    "R" '((lambda () (interactive)
            (load-file "~/.config/emacs/init.el"))
          :wk "Reload config")
    "," '((lambda () (interactive)
            (dired "~/Workspace/dotfiles/emacs/"))
          :wk "Open setting dir")
    ";" '(comment-line :wk "Comment lines")
    "w" '(save-buffer :wk "Save buffer")
    "q" '(save-buffers-kill-terminal :wk "Quit")
    "K" '(save-buffers-kill-emacs :wk "Kill emacs"))

  (vk/leader-keys
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

  (vk/leader-keys
    "c" '(:ignore t :wk "Code")
    "c A" '(lsp-execute-code-action :wk "Execute code")
    "c d" '(lsp-describe-thing-at-point :wk "Describe symbol")
    "c l" '(flycheck-list-errors :wk "List all errors")
    "c f" '(lsp-format-region :wk "Code format")
    "c r" '(lsp-rename :wk "Symbol rename")
    "c s" '(consult-lsp-symbols :wk "List symbols"))

  (vk/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d w" '(wdired-change-to-wdired-mode :wk "Wdired change names"))

  (vk/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (vk/leader-keys
    "f" '(:ignore t :wk "Files")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f e" '(neotree-toggle :wk "Exploer file viewer")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (vk/leader-keys
    "g" '(:ignore t :wk "Goto")
    "g e" '(consult-compile-error :wk "Goto compile error")
    "g f" '(consult-flycheck :wk "Goto flycheck")
    "g g" '(consult-goto-line :wk "Goto line")
    "g i" '(consult-imenu :wk "Goto imenu")
    "g I" '(consult-imenu-multi :wk "Goto multi imenu")
    "g k" '(consult-global-mark :wk "Goto global mark")
    "g o" '(consult-outline :wk "Goto outline")
    "g m" '(consult-mark :wk "Goto mark"))

  (vk/leader-keys
    "m" '(:ignore t :wk "Markfile")
    "m d" '(bookmark-delete :wk "Delete bookmark")
    "m l" '(list-bookmarks :wk "List bookmarks")
    "m m" '(bookmark-set :wk "Set bookmark")
    "m w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))
  
  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (vk/leader-keys
    "p" '(projectile-command-map :wk "Project"))

  ;; For rust language special
  (vk/leader-keys
    "r" '(:ignore t :wk "Rust Language")
    "r c" '(rustic-cargo-plain-run :wk "Cargo run with arguments"))

  (vk/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s f" '(consult-find :wk "Find")
    "s F" '(consult-locate :wk "Locate a file")
    "s g" '(consult-grep :wk "Grep")
    "s G" '(consult-git-grep :wk "Git grep")
    "s k" '(consult-keep-lines :wk "Keep lines")
    "s l" '(consult-line :wk "Line")
    "s L" '(consult-line-multi :wk "Multi line")
    "s m" '(consult-man :wk "Man pages")
    "s o" '(isearch-occur :wk "Occur")
    "s O" '(consult-multi-occur :wk "Multi occur")
    "s r" '(query-replace :wk "Query replace")
    "s R" '(consult-ripgrep :wk "Ripgrep")
    "s s" '(isearch-yank-symbol :wk "Yank symbol")
    "s t" '(tldr :wk "Lookup TLDR docs")
    "s u" '(consult-focus-lines :wk "Focus lines")
    "s w" '(woman :wk "Similar to man"))

  (vk/leader-keys
    "v" '(:ignore t :wk "Windows")
    ;; Window splits
    "v c" '(evil-window-delete :wk "Close window")
    "v n" '(evil-window-new :wk "New window")
    "v s" '(evil-window-split :wk "Horizontal split window")
    "v v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "v h" '(evil-window-left :wk "Window left")
    "v j" '(evil-window-down :wk "Window down")
    "v k" '(evil-window-up :wk "Window up")
    "v l" '(evil-window-right :wk "Window right")
    "v w" '(evil-window-next :wk "Goto next window"))

  ;; Using Emacs keybinding
  (vk-help-key
    "" '(:ignore t :wk "Help")
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
    "x" '(describe-command :wk "Display full documentation for command")))


(provide 'vk-keybind)
;;; vk-keybind.el ends here
