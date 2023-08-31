;; Suppress warnings
(eval-when-compile (require 'custom-setup))

(defvar socks-noproxy)
(defvar socks-server)

;; Function
(defun vk/icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and vk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icon nil t))))
;; Font
(defun vk/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Newline behaviour
(defun vk/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; Adjust Opacity - This function from purcell.
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

;; File and buffer
(defun vk/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun vk/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun vk/rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun vk/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'vk/reload-init-file #'reload-init-file)

;; Open custom file
(defun vk/find-custom-file()
  "Open custom files."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p vk-custom-file)
        (copy-file vk-custom-file custom-file)
      (user-error "The file `%s' doesn't exist" vk-custom-file)))
  (when (file-exists-p custom-file)
    (find-file custom-file)))

;; Misc
(defun vk/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun vk/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun vk/save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

;; Frame
(defvar vk/frame--geometry nil)
(defun vk/frame--save-geometry ()
  "Save current frame's geometry."
  (setq vk/frame--geometry
        `((left   . ,(frame-parameter nil 'left))
          (top    . ,(frame-parameter nil 'top))
          (width  . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

(defun vk/frame--fullscreen-p ()
  "Return Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun vk/frame-maximize ()
  "Maximize the frame."
  (interactive)
  (vk/frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun vk/frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil vk/frame--geometry))

(defun vk/frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (vk/frame--fullscreen-p)
    (vk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun vk/frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (vk/frame--fullscreen-p)
    (vk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun vk/frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (vk/frame--fullscreen-p)
    (vk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun vk/frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (vk/frame--fullscreen-p)
    (vk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;; Network Proxy
(defun vk/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" vk-proxy)
    (message "No HTTP proxy")))

(defun vk/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,vk-proxy)
          ("https" . ,vk-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (vk/proxy-http-show))

(defun vk/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (vk/proxy-http-show))

(defun vk/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (vk/proxy-http-disable)
    (vk/proxy-http-enable)))

(defun vk/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun vk/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string vk-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" vk-socks-proxy))
  (vk/proxy-socks-show))

(defun vk/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (vk/proxy-socks-show))

(defun vk/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (vk/proxy-socks-disable)
    (vk/proxy-socks-enable)))

(provide 'func-setup)