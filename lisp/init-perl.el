;;; package -- Summary
;;; #init-perl.el
;;; Commentary:
;;; Set up Perl mode
;;; Code:
(use-package cperl-mode
  :mode "\\.p[lm]\\'"
  :interpreter "perl"
  :config
  (setq cperl-hairy t))
(provide 'init-perl)
;;; init-perl.el ends here
