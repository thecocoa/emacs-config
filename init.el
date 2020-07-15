;;; init.el --- Felix's Emacs configuration
;;; COMMENTARY:
;;; CODE:

(defvar *weilbach/loading-epoch*
  (current-time)
  "The epoch of loading init.el.")

(defvar weilbach/emacs-config-dir
  (substring user-init-file 0 (- (length user-init-file) 8))
  "Directory where the Emacs configuration is stored.")

;; Bring necessary configuration files into scope
(add-to-list 'load-path (concat weilbach/emacs-config-dir "/config"))

;; Set file for custom
(setq custom-file (concat weilbach/emacs-config-dir "/.emacs-custom.el"))
(load custom-file)

(require 'weilbach-functions)
(weilbach/setup-use-package)

(require 'weilbach-config-general)
(require 'weilbach-config-font)
(require 'weilbach-config-theme)
(require 'weilbach-config-helm)
(require 'weilbach-config-lsp)
(require 'weilbach-config-lang)

(message "Loading init.el ... done (%.5fs)"
         (float-time (time-subtract (current-time) *weilbach/loading-epoch*)))

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
