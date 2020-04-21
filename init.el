;;; init.el --- Felix's Emacs configuration
;;; COMMENTARY:
;;; CODE:

;; Bring necessary configuration files into scope
(add-to-list 'load-path "~/.config/emacs/config")

(require 'weilbach-functions)

(weilbach/setup-use-package)
