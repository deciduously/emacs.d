;;; package -- Summary
;;; .# init-magit.el
;;; Commentary:
;;; Magit is an Emacs interface to Git.
;; (It's awesome)
;; https://github.com/magit/magit
;;; Code:
(use-package magit
  :diminish auto-revert-mode
  :bind
  (("C-c C-g" . magit-status)
   :map magit-status-mode-map
   ("q"       . magit-quit-session))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    "Make magit-status run alone in a frame."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))
(provide 'init-magit)
;;; init-magit.el ends here


