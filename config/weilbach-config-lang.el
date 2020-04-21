;;; weilbach-config-lang.el --- Languages configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

;;; C++
(use-package cc-mode
  :bind
  (:map c++-mode-map
        ("<f6>" . ff-find-other-file))
  (:map c-mode-map
        ("<f6>" . ff-find-other-file))
  :hook
  (c-mode-hook . lsp-deferred)
  (c++-mode-hook . lsp-deferred))


(use-package ccls
  :config
  (progn
    (setq-default
     flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))
  :custom
  (ccls-sem-highlight-method 'font-lock))

(use-package clang-format+
  :hook
  (c-mode-common-hook . clang-format+-mode))

(provide 'weilbach-config-lang)

;;; weilbach-config-lang.el ends here
