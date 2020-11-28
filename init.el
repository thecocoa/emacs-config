;;; init.el --- Felix's Emacs configuration
;;; COMMENTARY:
;; Byte compile directory C-u 0 M-x byte-recompile-directory
;;; CODE:

(defvar loading-epoch
  (current-time)
  "The epoch of loading init.el.")

;; Bring necessary configuration files into scope
(add-to-list 'load-path (concat user-emacs-directory "config"))

;; Set file for custom
(setq-default custom-file (concat user-emacs-directory ".emacs-custom.el"))
(ignore-errors (load custom-file)) ;; It may not exist now

;; Maybe there is a user config
(ignore-errors (load (concat
                      user-emacs-directory
                      "user/user-config-before.el")))

(require 'weilbach-functions)
(weilbach/setup-use-package)

(require 'weilbach-config-general)
(require 'weilbach-config-modeline)
(require 'weilbach-config-font)
(require 'weilbach-config-theme)
(require 'weilbach-config-helm)
(require 'weilbach-config-lsp)
(require 'weilbach-config-lang)
(require 'weilbach-config-mu4e)
(require 'weilbach-config-misc)

;; Maybe there is a user config
(ignore-errors (load (concat
                      user-emacs-directory
                      "user/user-config-after.el")))

(message "Loading init.el ... done (%.5fs)"
         (float-time (time-subtract (current-time) loading-epoch)))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
