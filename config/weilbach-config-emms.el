;;; weilbach-config-emms.el --- Felix's emms configuration
;;; COMMENTARY:
;;; CODE:

;; Needs a music player installed, like mpg123, vlc, etc.
;; Needs tag tool exiftool
;; Run one time emms-add-directory-tree
(use-package emms
  :config
  (progn
    (require 'emms-setup)
    (emms-all)
    (emms-default-players)

    (setq emms-source-file-default-directory "~/Music/"
          emms-info-functions '(emms-info-exiftool)
          )
    ))

(provide 'weilbach-config-emms)

;;; weilbach-config-emms.el ends here
