;;; weilbach-config-mu4e.el --- Mu4e configuration for my Emacs
;;; COMMENTARY:
;; Setup mu:
;;
;; mu init --maildir=~/Maildir --my-address=felix.weilbach@t-online.de --my-address=feweilbach@gmail.com --my-address=felix.weilbach@student.uni-augsburg.de
;; mu index
;;
;;; CODE:

(use-package mu4e
  :ensure nil
  :bind
  (("<f12>" . #'mu4e))
  :config
  (progn
    (defun enter-mu4e-context-tonline ()
      (setq mu4e-sent-folder   "/T-Online/INBOX.Sent"
            mu4e-drafts-folder "/T-Online/INBOX.Drafts"
            mu4e-trash-folder  "/T-Online/INBOX.Trash"
            mu4e-refile-folder "/T-Online/INBOX.Archive"

            mu4e-maildir-shortcuts
            '((:maildir "/T-Online/INBOX"       :key ?i)
              (:maildir "/T-Online/INBOX.Sent"  :key ?s)
              (:maildir "/T-Online/INBOX.Trash" :key ?t)))

      (require 'smtpmail)
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("securesmtp.t-online.de" 465 nil nil))
            smtpmail-auth-credentials
            '(("securesmtp.t-online.de" 465 "felix.weilbach@t-online.de" nil))
            smtpmail-default-smtp-server "securesmtp.t-online.de"
            smtpmail-smtp-server "securesmtp.t-online.de"
            smtpmail-smtp-service 465
            smtpmail-stream-type 'ssl)

      )

    (defun enter-mu4e-context-gmail ()
      (setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail"
            mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
            mu4e-trash-folder  "/Gmail/[Gmail].Trash"
            mu4e-refile-folder "/Gmail/[Gmail].Archive"

            mu4e-maildir-shortcuts
            '((:maildir "/Gmail/INBOX"              :key ?i)
              (:maildir "/Gmail/[Gmail].Sent Mail"  :key ?s)
              (:maildir "/Gmail/[Gmail].Trash"      :key ?t)
              (:maildir "/Gmail/[Gmail].All Mail"   :key ?a)))

      (require 'smtpmail)
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials
            '(("smtp.gmail.com" 587 "feweilbach@gmail.com" nil))
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587
            smtpmail-stream-type nil))

    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name "T-Online"
              :enter-func (lambda () (progn
                                       (mu4e-message "Entering T-Online Context")
                                       (enter-mu4e-context-tonline)
                                       ))
              :leave-func (lambda () (mu4e-message "Leave T-Online Context"))
              ;; We match based on the contact-fields of the message
	            :match-func (lambda (msg)
			                      (when msg
			                        (mu4e-message-contact-field-matches msg
			                          :to "felix.weilbach@t-online.de")))
              :vars '((user-mail-address . "felix.weilbach@t-online.de"  )
		                  (user-full-name . "Felix Weilbach" )
		                  ;; (mu4e-compose-signature .
		                  ;;                         (concat
		                  ;;                          "Alice Derleth\n"
		                  ;;                          "Lauttasaari, Finland\n"))
                      ))

            ,(make-mu4e-context
              :name "Gmail"
              :enter-func (lambda () (progn
                                       (mu4e-message "Entering Gmail Context")
                                       (enter-mu4e-context-gmail)
                                       ))
              :leave-func (lambda () (mu4e-message "Leave Gmail Context"))
              ;; We match based on the contact-fields of the message
	            :match-func (lambda (msg)
			                      (when msg
			                        (mu4e-message-contact-field-matches msg
			                          :to "feweilbach@gmail.com")))
              :vars '((user-mail-address . "feweilbach@gmail.com"  )
		                  (user-full-name . "Felix Weilbach" )
                      ))
            ))

    (setq mu4e-context-policy 'pick-first
          mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 120
          mu4e-sent-messages-behavior 'delete
          mu4e-use-maildirs-extension t
          mu4e-enable-async-operations t
          mu4e-attachment-dir "~/Downloads"
          mu4e-confirm-quit nil
          mu4e-view-html-plaintext-ratio-heuristic  most-positive-fixnum

          ;; mu4e-compose-context-policy nil
          )

    (add-hook 'mu4e-compose-mode-hook '(lambda ()
                                         (progn
                                           (flyspell-mode)
                                           (ispell-change-dictionary "de_DE")
                                           )))
    )
  )

(use-package mu4e-alert
  :config
  (progn
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    ))


;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'weilbach-config-mu4e)

;;; weilbach-config-mu4e.el ends here
