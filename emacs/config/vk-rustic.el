;; vk-rustic.el --- vk-rustic configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; Rust
(use-package rustic
  :general (:keymaps 'rustic-mode-map
                     "SPC cg" #'rustic-cargo-plain-run)
  :config
  (push 'rustic-clippy flycheck-checkers)
  (setq rustic-flycheck-clippy-params-nightly "--message-format=json -Zunstable-options")
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook))

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))

(use-package rust-playground)

(provide 'vk-rustic)

;;; vk-rustic.el ends here

