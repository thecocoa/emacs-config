;;; functions.el --- Functions that are commonly used
;;; COMMENTARY:
;;; CODE:

;(require 'projectile)
(require 'recentf)
(require 'dired)

;; External packages
(defvar weilbach/packages-arch-linux '() "Packages to install on Arch Linux.")
(defvar weilbach/packages-fedora '() "Packages to install on Fedora.")
(defvar weilbach/packages-python '() "Packages to install for python.")
(defvar weilbach/packages-node '() "Packages to install for node.")

(defun weilbach/add-package-arch-linux (package)
  "Add PACKAGE to WEILBACH/PACKAGES-ARCH-LINUX."
  (add-to-list 'weilbach/packages-arch-linux package))

(defun weilbach/add-package-fedora (package)
  "Add PACKAGE to WEILBACH/PACKAGES-FEDORA."
  (add-to-list 'weilbach/packages-fedora package))

(defun weilbach/add-package-python (package)
  "Add PACKAGE to WEILBACH/PACKAGES-PYTHON."
  (add-to-list 'weilbach/packages-python package))

(defun weilbach/add-package-node (package)
  "Add PACKAGE to WEILBACH/PACKAGES-NODE."
  (add-to-list 'weilbach/packages-node package))

(defun weilbach/install-packages-arch-linux ()
  "Install all packages from WEILBACH/PACKAGES-ARCH-LINUX."
  (interactive)
  (let* ((package-list (string-join weilbach/packages-arch-linux " ")))
    (async-shell-command (concat "pacaur -S " package-list))))

(defun weilbach/install-packages-fedora ()
  "Install all packages from WEILBACH/PACKAGES-FEDORA."
  (interactive)
  (let* ((package-list (string-join weilbach/packages-fedora " ")))
    (async-shell-command (concat "dnf install " package-list))))

(defun weilbach/install-packages-python ()
  "Install all packages from WEILBACH/PACKAGES-PYTHON."
  (interactive)
  (let* ((package-list (string-join weilbach/packages-python " ")))
    (async-shell-command (concat "pip --user install " package-list))))

(defun weilbach/install-packages-node ()
  "Install all packages from WEILBACH/PACKAGES-NODE."
  (interactive)
  (let* ((package-list (string-join weilbach/packages-node " ")))
    (async-shell-command (concat "npm i -g " package-list))))

(defun weilbach/setup-use-package ()
  "Setup use-package."
  (require 'package)
  (package-initialize)

  ;; (add-to-list 'package-archives
  ;;            '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq-default use-package-always-ensure t))

;;;###autoload
(defun weilbach/reload-init-file ()
  "Reloads the Emacs configuration file."
  (interactive)
  (load-file user-init-file))

;;;###autoload
(defun weilbach/kill-buffer-active ()
  "Kill the active buffer."
  (interactive)
  (kill-this-buffer))

;;;###autoload
(defun weilbach/delete-file (filename &optional ask-user)
  "Remove specified FILENAME or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal.  Stolen from spacemacs."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

;;;###autoload
(defun weilbach/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for a new name.

Also renames associated buffers (if any exists), invalidates
projectile cache and updates recentf list.  Stolen from spacemacs."
  (interactive "f")
  (require 'dired)
  (when (and filename (file-exists-p filename))
    (let* ((is-dir (file-directory-p filename))
           (short-name
            (if is-dir
                (file-name-base (directory-file-name filename))
              (file-name-nondirectory filename)))
           (new-filename
            (if new-filename new-filename
              (read-file-name
               (format "Rename %s to: " short-name)))))

      ;; Rename filename to new-filename and error if new-filename already
      ;; exists. `dired-rename-file' handles renaming of directories and files.
      ;; It updates the name of all associated buffers.
      (dired-rename-file filename new-filename nil)

      ;; Update recentf list.
      (when (fboundp 'recentf-add-file)
        (seq-map
         (lambda (fp)
           (recentf-add-file
            (concat new-filename (string-remove-prefix filename fp)))
           (recentf-remove-if-non-kept fp))
         (seq-filter
          (lambda (fp)
            (string-prefix-p filename fp))
          recentf-list)))

      ;; Invalidate projectile cache.
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache))

      ;; Inform user about tremendous success.
      (message "%s '%s' successfully renamed to '%s'"
               (if is-dir "Directory" "File")
               short-name
               (file-name-nondirectory new-filename)))))

(defvar weilbach/allowed-non-code-buffers '("*scratch*")
  "Allowed non code buffers.
This buffers don't get skipped in WEILBACH/NEXT-CODE-BUFFER and
WEILBACH/PREVIOUS-CODE-BUFFER")

(defun weilbach/next-code-buffer ()
  "Next buffer.  Skip buffer with **.
Buffers in WEILBACH/ALLOWED-NON-CODE-BUFFERS will not be skipped"
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (not (member (buffer-name) weilbach/allowed-non-code-buffers))
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun weilbach/previous-code-buffer ()
  "Previous buffer.  Skip buffer with **.
Buffers in WEILBACH/ALLOWED-NON-CODE-BUFFERS will not be skipped"
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (not (member (buffer-name) weilbach/allowed-non-code-buffers))
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))

;;;###autoload
(defun weilbach/open-init-file ()
  "Open the init file."
    (interactive)
   (find-file user-init-file))

(provide 'weilbach-functions)

;;; weilbach-functions.el ends here
