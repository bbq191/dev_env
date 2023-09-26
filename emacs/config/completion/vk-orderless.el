;; vk-orderless.el --- init configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'vk-orderless)

;;; vk-orderless.el ends here
