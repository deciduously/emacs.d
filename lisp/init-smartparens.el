;;; #init-smartparens.el
;;; Commentary:
;;; Code:
(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'rustic-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))
(provide 'init-smartparens)
;;; init-smartparens.el ends here
