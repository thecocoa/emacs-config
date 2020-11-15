;;; weilbach-dark-theme.el --- Felix's dark theme
;;; COMMENTARY:
;; Byte compile directory C-u 0 M-x byte-recompile-directory
;;; CODE:

;; (setq custom--inhibit-theme-enable nil)

(deftheme weilbach-dark "Weilbach theme, dark version")

(custom-theme-set-faces 'weilbach-dark
                        '(default ((t (:foreground "snow3" :background "#2C2F33" :bold nil))) t)

                        '(font-lock-function-name-face ((t (:foreground "MediumPurple" :bold t))) t)
                        '(font-lock-keyword-face ((t (:foreground "#7289DA" :bold t))) t)
                        '(font-lock-type-face ((t (:foreground "gold3" :bold t))) t)
                        '(font-lock-comment-face ((t (:foreground "IndianRed" :bold nil :slant italic))) t)
                        '(font-lock-string-face ((t (:foreground "SeaGreen" :bold nil))) t)
                        '(font-lock-constant-face ((t (:foreground "DarkCyan"))) t)

                        '(mode-line ((t (:foreground "snow3" :background "#23272A"))) t)
                        '(mode-line-inactive ((t (:foreground "snow3" :background "#23272A"))) t)
                        '(fringe ((t (:foreground "snow3" :background "#23272A"))) t)
                        '(vertical-border ((t (:foreground "#23272A" :background "#23272A"))) t)

                        '(company-tooltip ((t (:foreground "snow3" :background "#23272A"))) t)
                        '(company-tooltip-selection ((t (:foreground "snow3" :background "OliveDrab"))) t)
                        '(company-tooltip-common ((t (:foreground "snow2"))) t)
                        '(company-scrollbar-bg ((t (:background "#23272A"))) t)
                        '(company-scrollbar-fg ((t (:background "snow3"))) t)

                        '(hl-line ((t (:background "#23272A"))))
                        '(cursor ((t (:background "snow3"))))

                        '(flycheck-error ((t (:underline '(:color "red" :style line)))))
                        '(flycheck-warning ((t (:underline '(:color "orange" :style line)))))
                        '(flycheck-info ((t (:underline '(:color "DeepSkyBlue" :style line)))))
                        '(flycheck-fringe-error ((t (:foreground "red" :bold t))))
                        '(flycheck-fringe-warning ((t (:foreground "orange" :bold t))))
                        '(flycheck-fringe-info ((t (:foreground "DeepSkyBlue" :bold t))))

                        '(helm-ff-directory ((t (:foreground "#7289DA" :background "#2C2F33" :bold t))))
                        '(helm-selection ((t (:background "#23272A"))))
                        '(helm-source-header ((t (:background "MediumPurple" :bold t))))
                        '(helm-candidate-number ((t (:background "#23272A"))))
                        )

(provide 'weilbach-dark-theme)

;;; weilbach-dark-theme.el ends here
