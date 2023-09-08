;; vk-marginalia.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))


(provide 'vk-marginalia)

;;; vk-marginalia.el ends here
