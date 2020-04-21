;;; functions.el --- Functions that are commonly used
;;; COMMENTARY:
;;; CODE:


(defun weilbach/setup-use-package ()
  (require 'package)
  (package-initialize)

  (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))

  (require 'use-package)
  (setq use-package-always-ensure t))

(provide 'weilbach-functions)

;;; functions.el ends here
