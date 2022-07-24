;;; #init-neotree.el
;;; Commentary:
;;; Code:
(use-package neotree
  :init
  (require 'neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  )
(provide 'init-neotree)
;;; init-neotree.el ends here
