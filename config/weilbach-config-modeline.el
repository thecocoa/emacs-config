;;; weilbach-config-modeline.el --- Modeline configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(defun weilbach/flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning)
                       (format (concat (propertize "·%s" 'face 'flycheck-fringe-error)
                                       (propertize "·%s" 'face 'flycheck-fringe-warning)
                                       (propertize "·%s" 'face 'flycheck-fringe-info))
                               (or .error 0) (or .warning 0) (or .info 0))
                     "")))
                (`interrupted ".")
                (`suspicious "?"))))
    (concat " " flycheck-mode-line-prefix text)))

;; Configure mode line
(setq-default mode-line-format
              (list "%e"
                    'mode-line-front-space
                    'mode-line-mule-info
                    'mode-line-client
                    'mode-line-modified
                    'mode-line-remote
                    'mode-line-frame-identification
                    "%l:%C"
                    " "
                    'mode-line-buffer-identification
                    " "
                    'mode-line-percent-position
                    " "
                    '(:eval (propertize mode-name 'face 'mode-line-emphasis))
                    '(vc-mode vc-mode)
                    '(:eval (weilbach/flycheck-mode-line-status-text))
                    'mode-line-misc-info
                    'mode-line-end-spaces
                    ))

;; Show column numbers
(column-number-mode)

(provide 'weilbach-config-modeline)

;;; weilbach-config-modeline.el ends here
