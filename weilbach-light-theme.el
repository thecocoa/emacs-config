;;; weilbach-light-theme.el --- Felix's light theme
;;; COMMENTARY:
;; Byte compile directory C-u 0 M-x byte-recompile-directory
;;; CODE:

;; (setq custom--inhibit-theme-enable nil)

(deftheme weilbach-light "Weilbach theme, light version")

(custom-theme-set-faces 'weilbach-light
                        '(default ((t (:foreground "black" :background "white" :bold nil))) t)

                        '(font-lock-function-name-face ((t (:foreground "Purple" :bold t))) t)
                        '(font-lock-keyword-face ((t (:foreground "blue" :bold t))) t)
                        '(font-lock-type-face ((t (:foreground "ForestGreen" :bold t))) t)
                        '(font-lock-comment-face ((t (:foreground "Firebrick" :bold nil :slant italic))) t)
                        '(font-lock-string-face ((t (:foreground "SeaGreen" :bold nil))) t)
                        '(font-lock-constant-face ((t (:foreground "DarkCyan"))) t)

                        '(mode-line ((t (:foreground "black" :background "gainsboro"))) t)
                        '(mode-line-inactive ((t (:foreground "black" :background "gainsboro"))) t)
                        '(fringe ((t (:foreground "black" :background "gainsboro"))) t)
                        '(vertical-border ((t (:foreground "gainsboro" :background "gainsboro"))) t)

                        '(company-tooltip ((t (:foreground "black" :background "cornsilk"))) t)
                        '(company-tooltip-selection ((t (:foreground "black" :background "LightBlue"))) t)
                        '(company-tooltip-common ((t (:foreground "darkred"))) t)
                        '(company-scrollbar-bg ((t (:background "wheat"))) t)
                        '(company-scrollbar-fg ((t (:background "darkred"))) t)

                        '(hl-line ((t (:background "darkseagreen2"))))
                        '(cursor ((t (:background "black"))))

                                                '(flycheck-error ((t (:underline '(:color "red" :style line)))))
                        '(flycheck-warning ((t (:underline '(:color "orange" :style line)))))
                        '(flycheck-info ((t (:underline '(:color "DeepSkyBlue" :style line)))))
                        '(flycheck-fringe-error ((t (:foreground "red" :bold t))))
                        '(flycheck-fringe-warning ((t (:foreground "orange" :bold t))))

                        '(helm-ff-directory ((t (:foreground "#7289DA" :background "white" :bold t))))
                        '(helm-selection ((t (:background "darkseagreen2"))))
                        '(helm-source-header ((t (:background "ForestGreen" :bold t))))
                        '(helm-candidate-number ((t (:background "gainsboro"))))

                        '(mu4e-highlight-face ((t (:foreground "black" :background "gainsboro"))))
                        '(mu4e-header-key-face ((t (:foreground "DarkGreen" :bold t))))
                        )

(provide 'weilbach-light-theme)

;;; weilbach-light-theme.el ends here
