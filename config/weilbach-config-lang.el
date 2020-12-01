;;; weilbach-config-lang.el --- Languages configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

;;; C++
(use-package cc-mode
  :bind
  (:map c++-mode-map
        ("C-c s" . ff-find-other-file))
  (:map c-mode-map
        ("C-c s" . ff-find-other-file))
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred))
  )

(use-package ccls
  :config
  (progn
    (setq-default
     flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))
  :custom
  (ccls-sem-highlight-method 'font-lock))

(use-package clang-format+
  :hook
  ((c-mode . clang-format+-mode)
   (c++-mode . clang-format+-mode)))

(use-package python
  :hook
  ((python-mode . lsp-deferred))
  )

(use-package python-pytest
  :config
  (define-key python-mode-map (kbd "C-c t") 'python-pytest-dispatch)
  )

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
  :config
  (progn
    ;; Use pdf-tools to open PDF files
    (setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
                  TeX-source-correlate-start-server t)
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
    )
  )

(use-package latex
  :ensure auctex
  :hook
  ((LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . auto-fill-mode)))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :config
  (company-auctex-init))

;;; Kotlin
(use-package kotlin-mode)

;;; GLSL
(use-package glsl-mode)
(use-package company-glsl
  :config
  (add-to-list 'company-backends 'company-glsl))

;;; Cmake
(use-package cmake-mode
  :requires lsp-mode
  :hook
  lsp-deferred)

;;; Bash
(use-package sh-mode
  :ensure nil
  :requires lsp-mode
  :hook
  lsp-deferred)

;;; Fish
(use-package fish-mode)

;;; Rust
(use-package rust-mode)

;;; Meson
(use-package meson-mode)

;;; Json
(use-package json-mode)

;;; PlantUML
;; Requires plantuml
(use-package plantuml-mode
  :config
  (setq-default plantuml-default-exec-mode 'executable))
(use-package flycheck-plantuml
  :config
  (progn
    (flycheck-plantuml-setup)
    (setq-default flycheck-plantuml-executable "/usr/share/java/plantuml/plantuml.jar")
    )
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

;;; ZIMPL
(use-package zimpl-mode
  :load-path "external/zimpl-mode")

;; Graphviz
(use-package graphviz-dot-mode)

;; Haskell
(use-package haskell-mode)

;;; XML
(use-package nxml-mode
  :ensure nil
  :config
  (progn
    (require 'hideshow)
    (require 'sgml-mode)

    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"

                   "<!--"
                   sgml-skip-tag-forward
                   nil))



    (add-hook 'nxml-mode-hook 'hs-minor-mode)

    (define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)))

;;; Web Mode
(use-package web-mode
  :config
  (progn
    (setq web-mode-enable-engine-detection t)
    )
  :mode ("\\.html?\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         )
  )

;;; TypeScript
;; Requires typescript and typescript-language-server
;; Install: npm -g install typescript typescript-language-server
(use-package typescript-mode
    :hook
    (typescript-mode . lsp-deferred))

;; JavaScript
;; Requires typescript-language-server
;; Install: npm -g install typescript-language-server
(use-package js-mode
  :ensure nil
  :mode "\\.js\\'"
  :hook
  (js-mode . lsp-deferred)
  )

(use-package js2-mode
  :hook
  (js-mode . js2-minor-mode)
  )

;; Requires prettier
;; Install: npm -g install prettier
(use-package prettier-js
  :hook
  ((javascript-mode-hook . prettier-js-mode)
   (js2-mode-hook . prettier-js-mode)
   (typescript-mode-hook . prettier-js-mode)
   (web-mode-hook . prettier-js-mode))
  )

;;; C#
(use-package csharp-mode
  :hook
  ((csharp-mode . lsp-deferred)
   )
  )

;;; Lua
(use-package lua-mode)

;;; HLSL
(use-package hlsl-mode
  :load-path "external/hlsl-mode")

;;; Windows PowerShell
(use-package powershell)

;;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;; Hugo
(use-package hugo
  :load-path "external/hugo-el")

(provide 'weilbach-config-lang)

;;; weilbach-config-lang.el ends here
