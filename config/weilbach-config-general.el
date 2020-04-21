;;; weilbach-config-general.el --- General configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(set-frame-font "Source Code Pro 14")

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

              make-backup-files nil

              tab-width 2
              indent-tabs-mode nil)

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

;;; Org
(use-package org
  :hook
  (org-mode-hook . #'flyspell-mode)
  (org-mode-hook . #'auto-fill-mode))

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
  (global-flycheck-mode))

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

(provide 'weilbach-config-general)

;;; weilbach-config-general.el ends here
