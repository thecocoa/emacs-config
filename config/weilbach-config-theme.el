;;; weilbach-config-theme.el --- Theme configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(provide 'weilbach-config-theme)

(defvar weilbach/dark-theme 'spacemacs-dark "My dark theme.")
(defvar weilbach/light-theme 'spacemacs-light "My light theme.")

(defvar weilbach/dark-light-themes '(weilbach/dark-theme
                                     weilbach/light-theme
                                     )
  "Dark and light theme.  That can be toggeld with WEILBACH/TOGGLE-THEME.
First theme will be used by default")

(defvar weilbach/current-theme (car weilbach/dark-light-themes)
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

(defun weilbach/gnome-get-theme ()
  "Get the current set theme from Gnome."
  (substring
   (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")
   1
   -2))

(defun weilbach/gnome-is-dark-theme ()
  "Check if the current Gnome theme is a dark theme."
  (when (string-match-p "dark" (weilbach/gnome-get-theme)) t))

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

;; Set dark or light theme based on the OS theme
(when (eq system-type 'gnu/linux)
  (let*
      ((xdg-current-desktop (getenv "XDG_CURRENT_DESKTOP")))
    (cond
     ((string-equal xdg-current-desktop "GNOME")
      (progn
        (if (weilbach/gnome-is-dark-theme)
            (setq weilbach/current-theme weilbach/dark-theme)
          (setq weilbach/current-theme weilbach/light-theme)))
      ))))

;; Load default theme
(load-theme weilbach/current-theme t)

;;; weilbach-config-theme.el ends here
