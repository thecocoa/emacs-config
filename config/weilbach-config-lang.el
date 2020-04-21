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

;;; Python
(use-package python-mode
  :hook
  lsp-deferred)

;;; LaTeX
(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-syntactic-comment t)
  ;; Synctex support
  (TeX-source-correlate-start-server nil)
  ;; Don't insert line-break at inline math
  (LaTeX-fill-break-at-separators nil)
  )

(use-package latex
  :ensure auctex
  :hook
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . auto-fill-mode))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :config
  (company-auctex-init))

(provide 'weilbach-config-lang)

;;; weilbach-config-lang.el ends here
