;; vk-rustic.el --- vk-rustic configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

;; Rust
(use-package rustic
  :general (:keymaps 'rustic-mode-map :states 'normal
                     "SPC cg" #'rustic-cargo-plain-run))

(use-package rust-playground)

(provide 'vk-rustic)

;;; vk-rustic.el ends here

