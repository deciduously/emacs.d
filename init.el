;;; package -- Summary
;;; #init.el
;;; Commentary:
;;; Adopted from previous revision.
;;; Code:
;; How to manage backup files.  I just turned this feature off.
(setq delete-old-versions -1 )
;; Make numberic backup versions with no limit.
(setq version-control t )
;; backups of registered files are made as with other files.
(setq vc-make-backup-files t )
;; Follow the link and visit the real file.  Other values are "ask" or nil.
(setq vc-follow-symlinks t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
;; Don't show startup screen
(setq inhibit-startup-screen t )
;; silent bell on mistakes.
(setq ring-bell-function 'ignore )
;; force utf-8
(setq locale-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
;; toggle wrapping text at this column.
(setq-default fill-column 80)
;; Fill scratch with something.
(setq initial-scratch-message ";; deciduously-flavored emacs" )
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
(setq-default flycheck-emacs-lisp-load-path load-path)

;; Start stuff up
(use-package all-the-icons)

(require 'init-company)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-magit)
(require 'init-ivy)
(require 'init-lsp)
(require 'init-rust)
(require 'init-treemacs)
(require 'init-which-key)

(use-package js2-mode)
(use-package web-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit-blame treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs counsel ivy company flycheck neotree web-mode js2-mode which-key find-file-in-project all-the-icons use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here.
