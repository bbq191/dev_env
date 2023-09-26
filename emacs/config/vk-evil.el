;; vk-evil.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init (setq evil-want-integration t
        evil-want-keybinding nil
        ;; evil-vsplit-window-right 
        ;; evil-split-window-below
        evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  :hook (after-init . evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here
  ;; for documentation purposes in case you need it.
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

(use-package evil-tutor)

;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link  t)

(provide 'vk-evil)
;;; vk-evil.el ends here
