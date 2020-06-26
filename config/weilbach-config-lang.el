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
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred))
  )

(weilbach/add-package-arch-linux "ccls-git")
(weilbach/add-package-fedora "ccls")
(use-package ccls
  :requires lsp-mode
  :config
  (progn
    (setq-default
     flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))
  :custom
  (ccls-sem-highlight-method 'font-lock))

(weilbach/add-package-arch-linux "clang")
(weilbach/add-package-fedora "clang")
(use-package clang-format+
  :hook
  ((c-mode . clang-format+-mode)
   (c++-mode . clang-format+-mode)))

;;; Python
;; pip install python-language-server[all]
(weilbach/add-package-python "python-language-server[all]")
(use-package python-mode
  :requires lsp-mode
  :hook
  lsp-deferred)

;;; LaTeX
(weilbach/add-package-arch-linux "tex-live-most")
(weilbach/add-package-fedora "tex-live-scheme-full")
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
(weilbach/add-package-arch-linux "glslang")
(weilbach/add-package-fedora "glslang-devel")
(use-package glsl-mode)
(use-package company-glsl
  :config
  (add-to-list 'company-backends 'company-glsl))

;;; Cmake
(weilbach/add-package-python "cmake-language-server")
(use-package cmake-mode
  :requires lsp-mode
  :hook
  lsp-deferred)

;;; Bash
(weilbach/add-package-node "bash-language-server")
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
(weilbach/add-package-arch-linux "plantuml")
(weilbach/add-package-fedora "plantuml")
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable))
(use-package flycheck-plantuml
  :config
  (flycheck-plantuml-setup)
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

;;; ZIMPL
(use-package zimpl-mode
  :load-path "external/zimpl-mode")

;; Graphviz
(use-package graphviz-dot-mode)

;; Haskell
(use-package haskell-mode)

;; JavaScript
(use-package js2-mode)

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

(use-package web-mode)

(provide 'weilbach-config-lang)

;;; weilbach-config-lang.el ends here
