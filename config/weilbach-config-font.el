;;; weilbach-config-font.el --- Font configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(defvar weilbach/font-name "Source Code Pro 11" "Font to use in my Emacs.")

(defun weilbach/gnome-get-font-name ()
  "Get the name of the font when in Gnome desktop environment."
  (substring
   (shell-command-to-string
    "gsettings get org.gnome.desktop.interface monospace-font-name")
   1
   -2))

;; Set  WEILBACH/FONT-NAME according to the OS
(when (eq system-type 'gnu/linux)
  (let*
      ((xdg-current-desktop (getenv "XDG_CURRENT_DESKTOP")))
    (cond
     ((string-equal xdg-current-desktop "GNOME")
      (setq weilbach/font-name (weilbach/gnome-get-font-name))))))

;; Set the font
(set-frame-font weilbach/font-name)

(provide 'weilbach-config-font)

;;; weilbach-config-font.el ends here
