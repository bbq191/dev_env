;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

;; 判断是否是 macOS
(defconst vk-mac (eq system-type 'darwin)
  "是否运行在 Mac 系统下?")
(defconst vk-mac-gui
  (and (display-graphic-p) vk-mac)
  "是否运行于 Mac 图形界面下?")

;; 用户自定义配置文件
(defconst vk-custom-file
  (expand-file-name "vk-custom.el" user-emacs-directory)
  "Custom file of VK's Gnu Emacs.")

;; Custom group;;;;;;;;;;;;;;;;;;;;;
;; Theme;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vk-theme-alist
  '((default . doom-one)
    (pro     . doom-nord-light)
    (dark    . doom-vibrant)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-palenight)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'vk
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom vk-system-themes '((light . doom-one-light)
				              (dark  . doom-one))
  "List of themes related the system appearance.

It's only available on macOS currently."
  :group 'vk
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(defcustom vk-theme 'default
  "The color theme."
  :group 'vk
  :type `(choice (const :tag "System" system)
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    vk-theme-alist)
                 symbol))

;; org;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vk-org-directory (expand-file-name "~/Documents/org-notes")
  "Set org directory."
  :group 'vk
  :type 'string)

;; proxy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vk-http-proxy "127.0.0.1:1087"
  "Set HTTP/HTTPS proxy."
  :group 'vk
  :type 'string)

(defcustom vk-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy."
  :group 'vk
  :type 'string)

(defcustom vk-server t
  "Enable `server-mode' or not."
  :group 'vk
  :type 'boolean)

(defcustom vk-icon t
  "Display icons or not."
  :group 'vk
  :type 'boolean)

;; frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vk-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'vk
  :type 'boolean)

(defcustom vk-completion-style 'childframe
  "Completion display style."
  :group 'vk
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

;; prog;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vk-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'vk
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom vk-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'vk
  :type 'boolean)

(defcustom vk-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'vk
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom vk-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'vk
  :type 'boolean)

(defcustom vk-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'vk
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom vk-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+HEADERS"      . ?☰)
    ("#+OPTIONS:"     . ?⚙)
    ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'vk
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "vk-custom.el" user-emacs-directory))

(provide 'custom-setup)
