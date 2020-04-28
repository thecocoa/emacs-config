;;; functions.el --- Functions that are commonly used
;;; COMMENTARY:
;;; CODE:


(defun weilbach/setup-use-package ()
  "Setup use-package."
  (require 'package)
  (package-initialize)

  (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

(defun weilbach/set-frame-size (width height)
  "Set the current frames WIDTH and HEIGHT."
  (set-frame-size (selected-frame) width height))

(defun weilbach/set-frame-position (x y)
  "Set the current frame position to X and Y."
  (set-frame-position (selected-frame) x y))

(provide 'weilbach-functions)

;;; weilbach-functions.el ends here
