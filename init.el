;;; init.el --- Felix's Emacs configuration
;;; COMMENTARY:
;;; CODE:

;; Bring necessary configuration files into scope
(add-to-list 'load-path "~/.config/emacs/config")

;; Set file for custom
(setq custom-file "~/.config/emacs/.emacs-custom.el")
(load custom-file)

(require 'weilbach-functions)
(weilbach/setup-use-package)

(require 'weilbach-config-general)
(require 'weilbach-config-helm)

;;; init.el ends here
