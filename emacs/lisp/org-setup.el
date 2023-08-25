;; init-base.el --- Org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

;; Work Directory
(setq org-directory (concat (getenv "HOME") "/Documents/org-notes/"))

;; 可以不安装 org 包，但为了使用 org-load 这里必须提前安装 org-roam，安装 org-roam 会同时安装 org
;; 所以这里提前安装并设置了 org-mode
(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  ;; prettify
  (org-startup-indented t)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; image
  (org-image-actual-width nil)
  ;; more user-friendly
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  ;; call C-c C-o explicitly
  (org-return-follows-link nil)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; log
  (org-log-repeat 'time)
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto. We use minibuffer to filter instead of isearch.
  (org-goto-auto-isearch nil)
  ;; todo:
  ;;(org-goto-interface 'outline-path-completion)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://tools.ietf.org/html/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/"))))

;; Block until current queue processed.
(elpaca-wait)


;; Org-roam is a plain-text knowledge management system. It brings some of Roam's more powerful features into the Org-mode ecosystem.
;; 为了与内建 org 不发生版本冲突，必须在使用 org-babel-load-file 前安装好 org-roam 包。
(setq org-directory (concat (getenv "HOME") "/Documents/org-notes/"))

(use-package org-roam
  :after org
  :hook (after-init . org-roam-mode)	
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom (org-roam-directory (file-truename org-directory))
  :config (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

;; Block until current queue processed.
(elpaca-wait)
;; Toc-org
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Org-bullets - Org-bullets gives us attractive bullets rather than asterisks.
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :hook (org-mode-hook . (lambda () (org-bullets-mode 1))))

;;完全关闭 electric indent 自动缩进功能,防止自动缩进带来的意外问题。
(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)



(provide 'org-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
