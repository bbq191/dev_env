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

;; Custom group
(defcustom vk-org-directory (expand-file-name "~/Documents/org-notes")
  "Set org directory."
  :group 'vk
  :type 'string)

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

(defcustom vk-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
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
