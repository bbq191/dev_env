(defconst is-macsys (eq system-type 'darwin))

(defun vk/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
;; 按键绑定
(global-set-key (kbd "S-<return>") 'vk/newline-at-end-of-line)

(defun vk/reload-init-file ()
  "需要两次 load-file，否则不生效。"
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))
(global-set-key (kbd "C-c a r") 'vk/reload-init-file)

(defun vk/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
;; 调整界面 opacity
(global-set-key (kbd "M-C-8") (lambda () (interactive) (ikate/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (ikate/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
