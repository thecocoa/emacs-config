;;; weilbach-config-theme.el --- Theme configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(provide 'weilbach-config-theme)

(require 'flycheck)

(defvar weilbach/dark-theme 'spacemacs-dark "My dark theme.")
(defvar weilbach/light-theme 'spacemacs-light "My light theme.")

(defvar weilbach/dark-light-themes `(,weilbach/dark-theme
                                     ,weilbach/light-theme
                                    )
  "Dark and light theme.  That can be toggeld with WEILBACH/TOGGLE-THEME.
First theme will be used by default")

(defvar weilbach/current-theme (car weilbach/dark-light-themes)
  "Current theme.  Gets set by WEILBACH/TOGGLE-THEME.")

(defvar weilbach/alpha '(85 . 50)
  "Alpha value to use for transparency.")

(defun weilbach/toggle-transparency ()
  "Toggle between transparent and solid backgrund.
Transparency can be set by setting the variable WEILBACH/ALPHA."
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          weilbach/alpha '(100 . 100)))))

(global-set-key (kbd "C-c t") 'weilbach/toggle-transparency)

(defun weilbach/set-frame-size (width height)
  "Set the current frames WIDTH and HEIGHT."
  (set-frame-size (selected-frame) width height))

(defun weilbach/set-frame-position (x y)
  "Set the current frame position to X and Y."
  (set-frame-position (selected-frame) x y))

(defun weilbach/set-theme (theme)
  "Set the given THEME active."
  (load-theme theme t)

  ;; Set special colors for flycheck
  ;; (set-face-attribute
  ;;  'flycheck-error
  ;;  nil
  ;;  :foreground "white"
  ;;  :background "red"
  ;;  :underline nil)

  ;; (set-face-attribute
  ;;  'flycheck-warning
  ;;  nil
  ;;  :foreground "white"
  ;;  :background "orange"
  ;;  :underline nil)

  ;; (set-face-attribute
  ;;  'flycheck-info
  ;;  nil
  ;;  :foreground "white"
  ;;  :background "blue"
  ;;  :underline nil)
  )

;;;###autoload
(defun weilbach/toggle-theme ()
  "Toggle current theme."
  (interactive)
  (if (eq (car weilbach/dark-light-themes) weilbach/current-theme)
    (progn
      (weilbach/set-theme (cadr weilbach/dark-light-themes))
      (setq weilbach/current-theme (cadr weilbach/dark-light-themes)))
  (progn
    (weilbach/set-theme (car weilbach/dark-light-themes))
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

(defun weilbach/set-current-theme-based-on-os-theme ()
  "Set WEILBACH/CURRENT-THEME based on the os theme."
  (when (eq system-type 'gnu/linux)
  (let*
      ((xdg-current-desktop (getenv "XDG_CURRENT_DESKTOP")))
    (cond
     ((string-equal xdg-current-desktop "GNOME")
      (progn
        (if (weilbach/gnome-is-dark-theme)
            (setq weilbach/current-theme weilbach/dark-theme)
          (setq weilbach/current-theme weilbach/light-theme))))))))

(defun weilbach/check-for-os-theme-change ()
  "Check in endless loop if the theme of the OS has changed.
Set the theme if changed."
  (while t
    (let ((current-theme weilbach/current-theme))
      (progn
        (weilbach/set-current-theme-based-on-os-theme)
        (when (not (eq current-theme weilbach/current-theme))
          (weilbach/set-theme weilbach/current-theme))
        (sleep-for 2)))))

(setq-default custom-save-themes t)

(use-package spacemacs-theme
  :defer t)
(use-package vs-dark-theme)
(use-package vs-light-theme)

;; Set dark or light theme based on the OS theme

;; Load default theme
(weilbach/set-current-theme-based-on-os-theme)
(weilbach/set-theme weilbach/current-theme)

;; Check in background if the os theme changed
;; (make-thread 'weilbach/check-for-os-theme-change)

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

;; Set frame position and size
(when window-system
  (weilbach/set-frame-size 120 24)

  (let ((new-frame-position-x (- (/ (x-display-pixel-width) 2)
                                 (/  (frame-outer-width) 2)))
        (new-frame-position-y (- (/ (x-display-pixel-height) 2)
                                 (/ (frame-outer-height) 2))))

    (weilbach/set-frame-position new-frame-position-x
                                 new-frame-position-y)))

;;; weilbach-config-theme.el ends here
