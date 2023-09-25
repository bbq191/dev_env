;; vk-tramp.el  --- -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tramp)
(setq remote-file-name-inhibit-locks t)

;; Needs to be called from recentf's :init
;; todo: make this into a use-package invocation
(defun pt/customize-tramp ()

  (setq tramp-default-method "ssh"
        tramp-verbose 1
        remote-file-name-inhibit-cache nil
        tramp-use-ssh-controlmaster-options nil
        tramp-default-remote-shell "/bin/zsh"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/zsh")
          (shell-command-switch . "-c")))

  (connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
                                          '((shell-file-name . "/bin/zsh")
                                            (shell-command-switch . "-c"))))

(provide 'vk-tramp)
;;; vk-tramp.el ends here
