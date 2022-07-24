;;; package -- Summary
;;; #init-company.el
;;; Commentary:
;;; Install company-mode
;;; Code:
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(provide 'init-company)
;;; init-company.el ends here
