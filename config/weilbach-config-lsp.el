;;; weilbach-config-lsp.el --- LSP configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(use-package lsp-mode
  :config
  (progn
     )
  :custom
  (lsp-prefer-flymake nil)
  (lsp-log-io nil))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package company-lsp
  :config
  (progn
    (push 'company-lsp company-backends)))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(provide 'weilbach-config-lsp)

;;; weilbach-config-lsp.el ends here
