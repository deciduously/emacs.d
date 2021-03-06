;;; package -- Summary
;;; #init.el
;;; Commentary:
;;; Code:
(setq delete-old-versions -1 ) ; delete excess backups silently
(setq version-control t )
(setq vc-make-backup-files t )
(setq vc-follow-symlinks t )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore ) ; silent bell on mistakes
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq-default fill-column 80) ; toggle wrapping text at this column
(setq initial-scratch-message "EEEEEEEEEEEmacs...macs...(macs)... Hi Ben." )
(global-display-line-numbers-mode t )
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; use-package setup
(require 'package)
(setq package-enable-at-startup nil) ; dont do it immediately
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package

;; Define packages
(require 'use-package)

;; Always download if not available
(setq use-package-always-ensure t)

;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Pull in ocp-indent
(add-to-list 'load-path "/home/ben/.opam/4.07.0/share/emacs/site-lisp")
(setq-default flycheck-emacs-lisp-load-path load-path)

;; Start stuff up
(use-package all-the-icons)
(use-package find-file-in-project)
(require 'init-flycheck)
(require 'init-ivy)
(require 'init-company)
(require 'init-neotree)
(require 'init-which-key)
(require 'init-smartparens)
;; Programming mode
(use-package forth-mode)
(use-package js2-mode)
(use-package reason-mode)
(use-package web-mode)
(use-package ocp-indent)
(require 'init-clojure)
(require 'init-rust)
;; Pull in my crap
(require 'bl-fns)

;; KEYBINDINGS

(global-set-key [f8] 'neotree-project-dir)
(global-set-key (kbd "C-c q") (lambda ()
	       		       (interactive)
   			       (other-window -1)))
(global-set-key (kbd "C-c h") 'company-complete)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (forth-mode js2-mode web-mode flycheck-rust flycheck smartparens find-file-in-project fuzzy-find-in-project ffip-project-root counsel ivy which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
