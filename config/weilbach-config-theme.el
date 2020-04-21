;;; weilbach-config-theme.el --- Theme configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(provide 'weilbach-config-theme)


(defvar weilbach/dark-light-themes '(spacemacs-dark
                                     spacemacs-light
                                     )
  "Dark and light theme.  That can be toggeld with WEILBACH/TOGGLE-THEME.
First theme will be used by default")

(defvar weilbach/current-theme nil
  "Current theme.  Gets set by WEILBACH/TOGGLE-THEME.")

;;;###autoload
(defun weilbach/toggle-theme ()
  "Toggle current theme."
  (interactive)
  (if (eq (car weilbach/dark-light-themes) weilbach/current-theme)
    (progn
      (load-theme (cadr weilbach/dark-light-themes) t)
      (setq weilbach/current-theme (cadr weilbach/dark-light-themes)))
  (progn
    (load-theme (car weilbach/dark-light-themes) t)
    (setq weilbach/current-theme (car weilbach/dark-light-themes)))))

(setq-default custom-save-themes t
              weilbach/current-theme (car weilbach/dark-light-themes))

(use-package spacemacs-theme
  :defer t)

(use-package spaceline
  :config
  (progn
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode)
    (spaceline-info-mode)
    (spaceline-toggle-minor-modes-off)))

;; Load default theme
(load-theme (car weilbach/dark-light-themes) t)

;;; weilbach-config-theme.el ends here
