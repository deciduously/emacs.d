;;; package -- Summary
;;; #init-rust.el
;;; Commentary:
;;; Install all Rust related packages
;;;
;;; Code:
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c r l" . flycheck-list-errors)
              ("C-c r a" . lsp-execute-code-action)
              ("C-c r r" . lsp-rename)
              ("C-c r q" . lsp-workspace-restart)
              ("C-c r Q" . lsp-workspace-shutdown)
              ("C-c r s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'zz/rustic-mode-hook))

(defun zz/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package rust-playground)

(provide 'init-rust)
;;; init-rust.el ends here
