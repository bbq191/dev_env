;; vk-vc.el --- vc is awesome -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; Git
;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :config
  ;; Access Git forges from Magit
  (use-package forge
    :demand t
    :defines (forge-database-connector forge-topic-list-columns)
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init
    (setq forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
                                            (functionp 'emacsql-sqlite-builtin)
                                            (functionp 'sqlite-open))
                                       'sqlite-builtin
                                     'sqlite)
          forge-topic-list-columns
          '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
            ("Title" 60 t nil title  nil)
            ("State" 6 t nil state nil)
            ("Updated" 10 t nil updated nil)))))

;; Display transient in child frame
  (use-package transient-posframe
    :diminish
    :defines posframe-border-width
    :custom-face
    (transient-posframe ((t (:inherit tooltip))))
    (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
    :hook (after-init . transient-posframe-mode)
    :init
    (setq transient-posframe-border-width posframe-border-width
          transient-posframe-min-height nil
          transient-posframe-min-width 80
          transient-posframe-poshandler 'posframe-poshandler-frame-center
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    :config
    (with-no-warnings
      (defun my-transient-posframe--hide ()
        "Hide transient posframe."
        (posframe-hide transient--buffer-name))
      (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                :internal-border-color (face-background 'posframe-border nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-hide buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish t
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git configuration modes
(use-package git-modes)


(provide 'vk-vc)

;;; vk-vc.el ends here
