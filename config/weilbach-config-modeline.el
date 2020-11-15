;;; weilbach-config-modeline.el --- Modeline configuration for my Emacs
;;; COMMENTARY:
;;; CODE:

(eval-when-compile
  (require 'flycheck)

  (defvar mode-line-align-left)
  (defvar mode-line-align-middle)
  (defvar mode-line-align-right))

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

;; Code stolen from:
;; https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line

(defvar lunaryorn-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'lunaryorn-projectile-mode-line 'risky-local-variable t)

(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)

(setq mode-line-align-left
      '(" "
        "%e"
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        ;; (:eval (propertize mode-name 'face 'mode-line-emphasis))
        ;; FIXME: Causes slow down on TRAMP
        ;; lunaryorn-projectile-mode-line
        (vc-mode lunaryorn-vc-mode-line)
        " "
        mode-line-misc-info))

(setq mode-line-align-middle
      '(""
        " "
        "%l:%C"
        ))


(setq mode-line-align-right
      '(""
        " "
        (:eval (weilbach/flycheck-mode-line-status-text))
        " "
        mode-line-percent-position
        ))

(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))




(defconst WEILBACH/RIGHT_PADDING 1)

(defun reserve-left/middle ()
  "Reserve space for middle."
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  "Reserve space for right."
  (+ WEILBACH/RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

(setq-default mode-line-format
              (list
               mode-line-align-left
               '(:eval (mode-line-fill-center 'mode-line
                                              (reserve-left/middle)))
               mode-line-align-middle
               '(:eval
                 (mode-line-fill-right 'mode-line
                                       (reserve-middle/right)))
               mode-line-align-right
               ))

(provide 'weilbach-config-modeline)

;;; weilbach-config-modeline.el ends here
