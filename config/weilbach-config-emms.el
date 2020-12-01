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

    (setq )

    (setq emms-player-list '(emms-player-vlc
                             emms-player-mpg321
                             emms-player-mplayer)

          emms-source-file-default-directory "~/Music/"
          emms-info-functions '(emms-info-exiftool)
          emms-browser-covers 'emms-browser-cache-thumbnail-async
          )

    (global-set-key (kbd "C-c M") 'emms-smart-browse)
    (global-set-key (kbd "C-c m p") 'emms-pause)
    (global-set-key (kbd "C-c m n") 'emms-next)
    (global-set-key (kbd "C-c m b") 'emms-previous)
    ))

(provide 'weilbach-config-emms)

;;; weilbach-config-emms.el ends here
