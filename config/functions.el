;;; functions.el --- Functions that are commonly used
;;; COMMENTARY:
;;; CODE:

(defun weilbach/setup-use-package ()
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t))

(provide 'weilbach-functions)

;;; functions.el ends here
