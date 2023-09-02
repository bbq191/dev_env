(require 'func-setup)

(with-no-warnings
  ;; Key Modifiers
  (cond (vk-mac-gui
    ;; Compatible with Emacs Mac port
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          mac-right-option-modifier 'control)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo)))))

;; 调整界面 opacity
(global-set-key (kbd "M-C-8") (lambda () (interactive) (vk/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (vk/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "S-<return>") 'vk/newline-at-end-of-line)

;; Frame
(when (display-graphic-p)
  (and vk-mac-gui (bind-key "C-M-f" #'toggle-frame-fullscreen))

  ;; Resize and re-position frames conveniently
  ;; Same keybindings as Rectangle on macOS
  (bind-keys ("C-M-<return>"    . vk/frame-maximize)
             ("C-M-<backspace>" . vk/frame-restore)
             ("C-M-<left>"      . vk/frame-left-half)
             ("C-M-<right>"     . vk/frame-right-half)
             ("C-M-<up>"        . vk/frame-top-half)
             ("C-M-<down>"      . vk/frame-bottom-half)))

;; Global keybindings
(bind-keys ("s-r"     . vk/revert-this-buffer)
           ("C-x K"   . vk/delete-this-file)
           ("C-c C-l" . vk/reload-init-file))

(provide 'keybind-setup)

;;; keybind-setup.el ends here