;; vk-fix-default.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; There are a great many keybindings that are actively hostile,
;; in that they are bound to useless or obsolete functions that
;; are really easy to trigger accidentally.

;; These libraries are helpful to have around when writing little bits of elisp,
;; like the above. You can’t possibly force me to remember the difference
;; between the mapcar, mapc, mapcan, mapconcat, the cl- versions of some of the
;; aforementioned, and seq-map. I refuse. shut-up is good for noisy packages.
(use-package s)
(use-package dash :demand t :config (vk/unbind-bad-keybindings))
(use-package shut-up)

(bind-key* "C-h" #'backward-delete-char)

(bind-key "s-<up>" #'ff-find-related-file)
(bind-key "C-c a f" #'ff-find-related-file)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)
(bind-key "C-c q" #'fill-paragraph)

;; Very useful indent func for yanked after
(defun vk/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'vk/indent-just-yanked)

;; paste by auto format
(defun pt/yank ()
  "Call yank, then indent the pasted region, as TextMate does."
  (interactive)
  (let ((point-before (point)))
    (if mark-active (call-interactively 'delete-backward-char))
    (yank)
    (indent-region point-before (point))))

(bind-key "C-y" #'pt/yank)
(bind-key "s-v" #'pt/yank)
(bind-key "C-Y" #'yank)

(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-c s" #'isearch-forward-symbol)

(provide 'vk-fix-default)
;;; vk-fix-default.el ends here
