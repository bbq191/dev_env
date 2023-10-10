;; vk-fix-default.el --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Keep modeline clean.
(use-package diminish
  :config (diminish 'visual-line-mode))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  ;; After no-littering
  ;; Set user custom
  :config
  (setq custom-file (no-littering-expand-etc-file-name "vk-custom.el")))

;; MacOS specific
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-copy-env "HTTPS_PROXY")
  (exec-path-from-shell-copy-env "HTTP_PROXY")
  (exec-path-from-shell-copy-env "ALL_PROXY")
  (exec-path-from-shell-copy-env "LLVM_HOME")
  (exec-path-from-shell-copy-env "LDFLAGS")
  (exec-path-from-shell-copy-env "CPPFLAGS")
  (exec-path-from-shell-copy-env "RUSTUP_HOME"))

(use-package hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; There are a great many keybindings that are actively hostile,
;; in that they are bound to useless or obsolete functions that
;; are really easy to trigger accidentally.
(defun vk/unbind-bad-keybindings ()
  "Remove unhelpful keybindings."
  (-map (lambda (x) (unbind-key x))
        '("C-x C-r"         ;; find-file-read-only
          "C-x C-f"         ;; find-file
          "C-x C-d"         ;; list-directory
          "C-z"             ;; suspend-frame
          "C-x C-z"         ;; again
          "<mouse-2>"       ;; pasting with mouse-wheel click
          "<C-wheel-down>"  ;; text scale adjust
          "<C-wheel-up>"    ;; ditto
          "s-l"             ;; goto-line
          "s-w"             ;; delete-frame
          "s-n"             ;; make-frame
          "s-t"             ;; ns-popup-font-panel
          "s-p"             ;; ns-print-buffer
          "C-x C-q"         ;; read-only-mode
          "C-x C-c"         ;; quit emacs
          "C-h")))          ;; help

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

(provide 'vk-fix-default)
;;; vk-fix-default.el ends here
