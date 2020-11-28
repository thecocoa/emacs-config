;;; weilbach-config-general.el --- General configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(setq-default user-full-name "Felix Weilbach"
              user-mail-address "felix.weilbach@t-online.de"

              inhibit-startup-screen t
              frame-title-format '("%b - Emacs")
              cursor-type 'box
              show-trailing-whitespace t
              c-default-style "bsd"
              c-basic-offset 2
              visible-bell nil
              ring-bell-function 'ignore

              load-prefer-newer t

              scroll-margin 7
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              scroll-step 1
              compilation-scroll-output t

              tab-width 2
              require-final-newline t
              indent-tabs-mode nil
              mouse-yank-at-point t

              ;; For lsp
              garbage-collection-messages t
              gc-cons-threshold 100000000 ;; 100mb
              read-process-output-max (* 1024 1024) ;; 1mb

              initial-scratch-message nil
              initial-major-mode 'fundamental-mode

              )

;; Supress startup message
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))


(unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

(set-fringe-mode 0)
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

;; Make Emacs camel case aware
(global-subword-mode 1)

(recentf-mode)
(winner-mode)

(save-place-mode 1)

(put 'narrow-to-region 'disabled nil)

;; (windmove-default-keybindings)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Make compilation buffer appear at the bottom in extra buffer
(add-to-list 'display-buffer-alist
             `(,(rx bos "*compilation" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

;; Close compilation buffer on q
(add-hook 'compilation-mode-hook (lambda ()
                                   (local-set-key (kbd "q") #'kill-buffer-and-window)))

;; Make compilation buffer fullscreen on f
;;;###autoload
(defun compilation-buffer-fullscreen ()
  "Make current open compilation buffer fullscreen."
  (interactive)
  (select-window (previous-window))
  (switch-to-buffer "*compilation*")
  (delete-other-windows))

(add-hook 'compilation-mode-hook (lambda ()
                                   (local-set-key (kbd "f") #'compilation-buffer-fullscreen)))


;;; Keybindings
(global-set-key [remap kill-buffer] 'weilbach/kill-buffer-active)
(global-set-key [remap switch-to-buffer] 'ibuffer)
(global-set-key [remap next-buffer] 'weilbach/next-code-buffer)
(global-set-key [remap previous-buffer] 'weilbach/previous-code-buffer)
(global-set-key (kbd "C-.") 'weilbach/next-code-buffer)
(global-set-key (kbd "C-,") 'weilbach/previous-code-buffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-w") 'obar/kill-region-or-backward-word)

(global-set-key (kbd "M-S ;") 'comment-or-uncomment-region)
(global-set-key [remap newline] 'newline-and-indent)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x l") 'goto-line)

(global-set-key (kbd "C-c M") 'mpc)
(global-set-key (kbd "C-c m p") 'mpc-play)
(global-set-key (kbd "C-c m P") 'mpc-pause)
(global-set-key (kbd "C-c m n") 'mpc-next)
(global-set-key (kbd "C-c m b") 'mpc-prev)

;; Terminal
(defun weilbach/pop-shell (arg)
  "Pop a shell in a side window and ass ARG to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'eshell))
      (current-buffer))
    '((side . bottom)))))

(global-set-key (kbd "C-t") #'weilbach/pop-shell)

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")


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
    (global-company-mode)
    )
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

;;; Flycheck
(use-package flycheck
  :config
  (progn
    (global-flycheck-mode)

    (setq-default flycheck-mode-line-prefix "")

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
      :fringe-face 'flycheck-fringe-info)

    ))

(use-package flycheck-pos-tip
  :requires flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

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
  (progn
    (projectile-mode +1)
    )
  :bind
  (("M-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)
   ("C-c c" . projectile-compile-project)
   ("C-c r" . projectile-run-project)))

(use-package devhelp
  :load-path "external/devhelp")

(use-package string-inflection
  :bind
  (("C-c C-u" . #'string-inflection-cycle)))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package which-key
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-bottom)
            (setq which-key-idle-delay 3.0)))

(use-package pdf-tools
  :config (pdf-tools-install))

(use-package saveplace-pdf-view
  :config
  (progn
    (save-place-mode 1)
    ))

(use-package direnv
  :config
  (direnv-mode))

(provide 'weilbach-config-general)

;;; weilbach-config-general.el ends here
