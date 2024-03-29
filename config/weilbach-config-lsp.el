;;; weilbach-config-lsp.el --- LSP configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (progn
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    )
  :custom
  (lsp-prefer-flymake nil)
  (lsp-log-io nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-delay 2)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; (use-package company-lsp
;;   :config
;;   (progn
;;     (push 'company-lsp company-backends)))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

;; Install cpp dap tools with M-x dap-cpptools-setup
(use-package dap-mode
  :hook
  (lsp-mode . (lambda () (require 'dap-cpptools)))
  )

(provide 'weilbach-config-lsp)

;;; weilbach-config-lsp.el ends here
