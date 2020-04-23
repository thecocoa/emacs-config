;;; init.el --- Felix's Emacs configuration
;;; COMMENTARY:
;;; CODE:

(defvar weilbach/emacs-config-dir "~/.config/emacs"
  "Directory where the Emacs configuration is stored")

(when (version< emacs-version "27")
  (setq weilbach/emacs-config-dir "~/.emacs.d"))

;; Bring necessary configuration files into scope
(add-to-list 'load-path (concat weilbach/emacs-config-dir "/config"))

;; Set file for custom
(setq custom-file (concat weilbach/emacs-config-dir "/.emacs-custom.el"))
(load custom-file)

(require 'weilbach-functions)
(weilbach/setup-use-package)

(require 'weilbach-config-general)
(require 'weilbach-config-theme)
(require 'weilbach-config-helm)
(require 'weilbach-config-lsp)
(require 'weilbach-config-lang)

;;; init.el ends here
