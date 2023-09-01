;; funcs-setup.el --- Define functions. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

;; Suppress warnings
(require 'cl-lib) ;; cl-loop dependence
(eval-when-compile (require 'custom-setup))

(defvar socks-noproxy)
(defvar socks-server)

;; Function
;; Icon
(defun vk/icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and vk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icon nil t))))

(defun vk/treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

;; theme
(defun vk/theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme vk-theme-alist) Theme 'doom-one))

(defun vk/compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random system))
      (string-prefix-p "doom" (symbol-name (vk/theme-name theme)))))

(defun vk/dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun vk/theme-enable-p (theme)
  "The THEME is enabled or not."
  (and theme
       (not (memq vk-theme '(auto random system)))
       (memq (vk/theme-name theme) custom-enabled-themes)))

(defun vk/load-theme (theme)
  "Disable others and enable new THEME."
  (when-let ((theme (vk/theme-name theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun vk/load-system-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (vk/load-theme (alist-get appearance vk-system-themes)))

(defun vk/do-load-theme (theme &optional no-save)
  "Load color THEME. Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (completing-read "Load theme: "
                      `(system
                        ,@(mapcar #'car vk-theme-alist))))))

  ;; Disable time-switching themes
  (when (fboundp #'circadian-activate-latest-theme)
    (cancel-function-timers #'circadian-activate-latest-theme))

  ;; Disable system theme
  (when (bound-and-true-p auto-dark-mode)
    (setq auto-dark--last-dark-mode-state 'unknown)
    (auto-dark-mode -1))

  (pcase theme
    ('system
     ;; System-appearance themes
     (use-package auto-dark
       :ensure t
       :diminish
       :init
       (setq auto-dark-light-theme (alist-get 'light vk-system-themes)
             auto-dark-dark-theme (alist-get 'dark vk-system-themes))
       (when (and vk-mac (not (display-graphic-p)))
         (setq auto-dark-detection-method 'osascript))
       (auto-dark-mode 1)))
    (_
     (vk/load-theme theme)))

  ;; Set option
  (vk/set-variable 'vk-theme theme no-save))

;; Font
(defun vk/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Misc
(defun vk/set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun vk/too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 10000)
    (> (buffer-size) 100000)))

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

;; UI
(defvar vk/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun vk/run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'vk/after-load-theme-hook))
(advice-add #'load-theme :after #'vk/run-after-load-theme-hook)

(defun vk/icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and vk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun vk/childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun vk/childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq vk-completion-style 'childframe)
       (vk/childframe-workable-p)))

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
      (message "Current HTTP proxy is %s" vk-http-proxy)
    (message "No HTTP proxy")))

(defun vk/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,vk-http-proxy)
          ("https" . ,vk-http-proxy)
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

;;; func-setup.el ends here
