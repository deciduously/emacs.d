;;; package -- Sum
;;; #init-org.el
;;; Commentary:
;;; Install Org mode
;;; Code:
(use-package org
  ;;    :pin manual
  :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  :bind
  (:map org-mode-map
	(org-special-ctrl-a/e t)
	(org-special-ctrl-k t)
	("C-c l" . org-store-link)
	("A-h" . org-mark-element))
  :custom
  <<org-mode-custom-vars>>
  :custom-face
  <<org-mode-faces>>
  :hook
  <<org-mode-hooks>>
  :config
  (org-directory "~/org")
  (org-log-done t)
  (org-startup-indented t)
  (org-log-into-drawer t)
  (use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 4)))
;;; init-org.el ends here
