;;; weilbach-config-general.el --- General configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(setq-default user-full-name "Felix Weilbach"
              user-mail-address "felix.weilbach@t-online.de"

              inhibit-startup-screen t
              frame-title-format '("%b - Emacs")
              cursor-type 'bar
              show-trailing-whitespace t
              c-default-style "bsd"
              c-basic-offset 2

              scroll-margin 7
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              scroll-step 1
              compilation-scroll-output t

              make-backup-files nil

              tab-width 2
              indent-tabs-mode nil

              ;; gc-cons-threshold 1500000000
              )

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(global-auto-revert-mode t)

(recentf-mode)
(winner-mode)

(windmove-default-keybindings)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Make compilation buffer appear at the bottom in extra buffer
(add-to-list 'display-buffer-alist
             `(,(rx bos "*compilation" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(toggle-frame-maximized)

;;; Keybindings
(global-set-key [remap kill-buffer] 'weilbach/kill-buffer-active)
(global-set-key [remap switch-to-buffer] 'ibuffer)
(global-set-key [remap next-buffer] 'weilbach/next-code-buffer)
(global-set-key [remap previous-buffer] 'weilbach/previous-code-buffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "M-S ;") 'comment-or-uncomment-region)
(global-set-key [remap newline] 'newline-and-indent)

;;; Editing
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ace Window
(use-package ace-window
  :bind
  (("M-o" . #'ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö ?ä)))

;;; Org
(use-package org
  :hook
  (org-mode . flyspell-mode)
  (org-mode . auto-fill-mode))

(use-package org-ref)

;;; Company
(use-package company
  :config
  (progn
    (global-company-mode))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;; Flycheck
(use-package flycheck
  :config
  (progn
    (global-flycheck-mode)

    ;; Make fringe bitmap of flycheck pretty
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

;;; Magit
(use-package magit
  :bind
  (("C-x g" . magit-status)))

;;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; Smartparens
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)))

;;; Markdown
(use-package markdown-mode)

;;; Yaml
(use-package yaml-mode)

;;; Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  (("C-c p" . projectile-command-map)
   ("<f5>" . projectile-compile-project)
   ("C-<f5>" . projectile-run-project)))

(provide 'weilbach-config-general)

;;; weilbach-config-general.el ends here
