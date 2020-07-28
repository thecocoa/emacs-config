;;; weilbach-config-helm.el --- Helm configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(use-package helm
             :config
             (progn
               (setq-default helm-ff-lynx-style-map t)
               (helm-mode 1)

               ;; Make helm buffer appear at the bottom in extra buffer
               (add-to-list 'display-buffer-alist
                            `(,(rx bos "*helm" (* not-newline) "*" eos)
                              (display-buffer-in-side-window)
                              (inhibit-same-window . t)
                              (window-height . 0.4))))
             :bind
             (([remap execute-extended-command] . helm-M-x)
              ([remap find-file] . helm-find-files)
              ([remap list-buffers] . helm-buffers-list)
              ("C-x b" . helm-mini)
              ("C-c o" . helm-occur)
              ("M-/" . helm-dabbrev)
              ([remap recentf-open-files] . helm-recentf)
              ([remap yank-pop] . helm-show-kill-ring)
              ("C-M-s" . helm-projectile-grep)))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :custom
  (projectile-completion-system 'helm))

(provide 'weilbach-config-helm)

;;; weilbach-config-helm.el ends here
