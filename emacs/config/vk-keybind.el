;; vk-keybind.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package general
  :config
  (general-evil-setup)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
    "," '((lambda () (interactive)
            (dired "~/Workspace/dotfiles/emacs/"))
          :wk "Open user-emacs-directory in dired")
    "SPC" '(execute-extended-command :wk "Counsel M-x")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument")
    "q q" '(save-buffers-kill-terminal :wk "Save & Quit")
    "Q" '(save-buffers-kill-emacs :wk "Kill emacs"))

  (dt/leader-keys
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
  
  (dt/leader-keys
    "c" '(:ignore t :wk "Code")
    "c f" '(eglot-format-buffer :wk "Format current buffer")
    "c e l" '(flycheck-list-errors :wk "List all errors"))
  
  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree"))

  (dt/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (dt/leader-keys
    "f" '(:ignore t :wk "Files")
    "f f" '(find-file :wk "Find file")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
    "f l" '(counsel-locate :wk "Locate a file")
    "f r" '(counsel-recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (dt/leader-keys
    "g" '(:ignore t :wk "Goto")
    "g e" '(consult-compile-error :wk "Goto compile error")
    "g f" '(consult-flycheck :wk "Goto flycheck")
    "g g" '(consult-goto-line :wk "Goto line")
    "g o" '(consult-outline :wk "Goto outline")
    "g m" '(consult-mark :wk "Goto mark")
    "g k" '(consult-global-mark :wk "Goto global mark")
    "g i" '(consult-imenu :wk "Goto imenu")
    "g I" '(consult-imenu-multi :wk "Goto multi imenu"))
  
  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h :" '(consult-complex-command :wk "Repeat complex command")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  (dt/leader-keys
    "s" '(:ignore t :wk "Search")
    "s f" '(consult-find :wk "Find")
    "s g" '(consult-grep :wk "Grep")
    "s G" '(consult-git-grep :wk "Git grep")
    "s r" '(consult-ripgrep :wk "Ripgrep")
    "s l" '(consult-line :wk "Line")
    "s L" '(consult-line-multi :wk "Multi line")
    "s m" '(consult-multi-occur :wk "Multi occur")
    "s k" '(consult-keep-lines :wk "Keep lines")
    "s u" '(consult-focus-lines :wk "Focus lines")
    "s r" '(query-replace :wk "Query replace")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  (dt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Horizontal vsplit window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")))


;; Block until current queue processed.
(elpaca-wait)

(provide 'vk-keybind)

;;; vk-keybind.el ends here
