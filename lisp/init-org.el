;;; package -- Sum
;;; #init-org.el
;;; Commentary:
;;; Install Org mode
;;; Code:
(use-package org
  ;;    :pin manual
  :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c o" . (lambda () (interactive) (find-file "~/org/organizer.org")))
  :custom
   (org-use-speed-commands
	 (lambda ()
	   (and (looking-at org-outline-regexp)
		(looking-back "^\**"))))
  ;:custom-face
  ; <<org-mode-faces>>
  ;:hook
  ; <<org-mode-hooks>>
  :config
  (progn
    (setq org-directory "~/org")
    (setq org-log-done t)
    (setq org-startup-indented t)
    (setq org-log-into-drawer t)
    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)))

  (use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 4))
(provide 'init-org)
;;; init-org.el ends here
