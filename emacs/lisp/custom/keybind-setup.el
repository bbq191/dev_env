;; 调整界面 opacity
(global-set-key (kbd "M-C-8") (lambda () (interactive) (vk/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (vk/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "C-c a r") 'vk/reload-init-file)
(global-set-key (kbd "S-<return>") 'vk/newline-at-end-of-line)
