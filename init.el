;;; package -- Summary
;;; #init.el
;;; Commentary:
;;; Adopted from previous revision.
;;; Code:
; How to manage backup files.  I just turned this feature off.
(setq delete-old-versions -1 )
; Make numberic backup versions with no limit.
(setq version-control t )
; backups of registered files are made as with other files.
(setq vc-make-backup-files t )
; Follow the link and visit the real file.  Other values are "ask" or nil.
(setq vc-follow-symlinks t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
; Don't show startup screen
(setq inhibit-startup-screen t )
; silent bell on mistakes.
(setq ring-bell-function 'ignore )
; force utf-8
(setq locale-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
; toggle wrapping text at this column.
(setq-default fill-column 80)
; Fill scratch with something.
(setq initial-scratch-message "welcome to deciduously-flavored emacs" )
(global-display-line-numbers-mode t )
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
