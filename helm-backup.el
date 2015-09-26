;;; helm-backup.el --- Backup each file change using git

;; Copyright (C) 2013-2015 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/helm-backup
;; Version: 0.3.0
;; Package-Requires: ((helm "1.5.5"))
;; Keywords: backup

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To store change every time you save a file add :
;; (add-hook 'after-save-hook 'helm-backup-versioning)
;; or from Emacs you can do :
;; M-x customize-variable RET after-save-hook RET [INS] helm-backup-versioning

;; To retrieve file backup, from buffer call `helm-backup' :
;; M-x helm-backup
;; for convenience you can define key binding as follow :
;; (global-set-key (kbd "C-c b") 'helm-backup)
;;
;; To do:
;;
;; - I there a mean in git to create a tree object without using the
;; index?
;;
;; - Provide a command to allow deletion of files from history.
;;
;; - split into two files: the helm interface, and the backup mechanism


;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'cl-lib)


(defgroup helm-backup nil
  "Backup system using git and helm."
  :group 'helm)

(defcustom helm-backup-path (expand-file-name "~/.helm-backup/")
  "Backup location."
  :group 'helm-backup
  :type 'string)

(defvar helm-backup-use-hardlinks t)

(defvar helm-backup-namespace "refs/backup/")

(defvar helm-backup-ref "all")

(defcustom helm-backup-git-binary (executable-find "git")
  "Git binary path."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-list-format "%cd: %s  (%ar)"
  "Format use to display entries in helm buffer, follow git log format."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-excluded-entries nil
  "Define a list of file/folder regexp to exclude from backup.
/home/user/password => exclude password in /home/user
.*\\.el$ => exclude .el extension
/root/.* => exclude everything inside root
.*/password/.* => exclude all folders with name 'password'"
  :group 'helm-backup
  :type '(repeat regexp))

(defvar magit-backup-respect-git-branches t)

(defmacro helm-backup-in-backup-path (&rest body)
  (declare (debug t))
  `(let ((default-directory helm-backup-path))
     ,@body))

(defun magit-backup--strip-final-newline (output)
  (string-match (rx bos (group (*? anything)) (* (any "\n\r")) eos) output)
  (match-string 1 output)  )

(defun helm-backup-exec-git-command (command &rest args)
  "Execute git COMMAND and return result string, nil if failed."
  (let ((keep-final-newline nil))
    (when (memq 'keep-final-newline args)
      (setq keep-final-newline t
            args (delq 'keep-final-newline args)))
    (when-let ((output (with-temp-buffer
                         (when (zerop
                                (apply #'process-file helm-backup-git-binary nil t nil command args))
                           (buffer-string)))))
      (if keep-final-newline
          output
        (magit-backup--strip-final-newline output)))))

(defun helm-backup-git-command-return-value (command &rest args)
  (helm-backup-in-backup-path
    (apply #'process-file helm-backup-git-binary nil nil nil command args)))

(defun helm-backup-get-ref (file)
  (let ((default-directory (file-name-directory file)))
    (concat helm-backup-namespace
            (or (and magit-backup-respect-git-branches
                     (require 'vc nil t)
                     (vc-git-registered file)
                     (concat "brances/"
                             (file-name-nondirectory
                              (helm-backup-exec-git-command "symbolic-ref" "--quiet" "HEAD"))))
                helm-backup-ref))))

(defun helm-backup-verify-ref (ref)
  (helm-backup-in-backup-path
   (helm-backup-exec-git-command "rev-parse" "--quiet" "--verify" ref)))

(defun helm-backup-init-git-repository ()
  "Initialize git repository."
  (unless (file-directory-p helm-backup-path)
    (helm-backup-exec-git-command "init" helm-backup-path)
    (helm-backup-in-backup-path
      (helm-backup-exec-git-command "config" "--local" "user.email" "noemail@noemail.com")
      (helm-backup-exec-git-command "config" "--local" "user.name" "noname"))))

(defun helm-backup-transform-filename-for-git (filename)
  "Transform FILENAME to be used in git repository."
  (when (and filename (file-name-absolute-p filename))
    (file-relative-name filename "/")))

(defun helm-backup-copy-file-to-repository (filename)
  "Create folder in repository and copy file using FILENAME in it."
  (let ((directory (expand-file-name (file-name-directory
                                      (helm-backup-transform-filename-for-git filename))
                                     helm-backup-path)))
    (make-directory directory t)
    (if (or (not helm-backup-use-hardlinks)
            (file-remote-p filename))
        (copy-file filename directory t t t)
      (add-name-to-file filename (expand-file-name (file-name-nondirectory filename) directory) t))))

(defun helm-backup-get-backup-file-name (filename)
  (expand-file-name (file-name-nondirectory filename)
                    (concat helm-backup-path (file-name-directory filename))))

(defun helm-backup-file-excluded-p (file)
  "Check if a FILENAME is excluded from backup."
  (cl-some (lambda (regexp) (string-match-p (concat "\\`" regexp "\\'") file))
           helm-backup-excluded-entries))

(defun helm-backup-version-file (filename &optional message force)
  "Version file using FILENAME in backup repository.
Return non-nil when a commit has been made."
  (cl-callf or message "backup %s")
  (when (and filename
             (file-name-absolute-p filename)
             (file-exists-p filename)
             (not (helm-backup-file-excluded-p filename)))
    (helm-backup-init-git-repository)
    (helm-backup-copy-file-to-repository filename)
    (let ((ref (helm-backup-get-ref filename)))
      (helm-backup-in-backup-path
       (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
         (unless (helm-backup-verify-ref ref)
           ;; reset index and make initial void commit under ref
           (helm-backup-exec-git-command "rm" "--cached" "*")
           (helm-backup-exec-git-command
            "update-ref" ref
            (helm-backup-exec-git-command
             "commit-tree"
             "-m" (format "starting backing up in %s" ref)
             (helm-backup-exec-git-command "write-tree"))))
         ;; Prepare index.  Reset to appropriate head and add this file.
         (helm-backup-exec-git-command "reset" ref)
         (helm-backup-exec-git-command "add" filename-for-git)
         (when (or force
                   (= 1 (helm-backup-git-command-return-value
                         "diff" "--quiet"
                         ref "--" filename-for-git)))
           (let ((parent (if force
                             (nth 1 (split-string
                                     (helm-backup-exec-git-command
                                      "log" ref
                                      "-2"
                                      "--pretty=format:%H"
                                      filename-for-git)
                                     "\n"))
                           ref)))
             
             (helm-backup-exec-git-command
              "update-ref" ref
              (helm-backup-exec-git-command
               "commit-tree"
               "-m" (format message (file-name-nondirectory filename))
               "-p" parent
               (helm-backup-exec-git-command "write-tree"))))
           t))))))

(defvar-local helm-backup--not-first-save-p nil)

(defun helm-backup-before-save ()
  "Backup before save."
  (when (helm-backup-versioning
         (concat "%s before save"
                 (unless helm-backup--not-first-save-p
                   " (new session)")))
    (setq helm-backup--not-first-save-p t)))

(defun helm-backup-after-save ()
  "Backup after save."
  (when (helm-backup-versioning
         (concat "%s after save"
                 (unless helm-backup--not-first-save-p
                   " (new session)")))
    (setq helm-backup--not-first-save-p t)))

;;;###autoload
(define-minor-mode helm-backup-mode ()
  "Automatically backup files when saving.
$$$FIXME."
  :global t :group 'helm-backup
  (if helm-backup-mode
      (progn (add-hook 'after-save-hook  #'helm-backup-after-save)
             (add-hook 'before-save-hook #'helm-backup-before-save))
    (remove-hook 'after-save-hook  #'helm-backup-after-save)
    (remove-hook 'before-save-hook #'helm-backup-before-save)))

(defun helm-backup-list-file-change-time (filename)
  "Build assoc list using commit id and message rendering format using FILENAME."
  (let ((ref (helm-backup-get-ref filename)))
    (when (helm-backup-verify-ref ref)
      (helm-backup-in-backup-path
       (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
         (cl-mapcar #'cons
                    (split-string (helm-backup-exec-git-command "log"
                                                                ref
                                                                (format
                                                                 "--pretty=format:%s"
                                                                 helm-backup-list-format)
                                                                "--date=local"
                                                                filename-for-git)
                                  "\n")
                    (split-string (helm-backup-exec-git-command "log"
                                                                ref
                                                                "--pretty=format:%h"
                                                                filename-for-git)
                                  "\n")))))))

(defun helm-backup-fetch-backup-file (commit-id filename)
  "Retrieve content file from backup repository using COMMIT-ID and FILENAME."
  (helm-backup-in-backup-path
   (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
     (when (and commit-id
                (not (string= (helm-backup-exec-git-command "log" "--ignore-missing" "-1"
                                                            commit-id "--" filename-for-git)
                              "")))
       (helm-backup-exec-git-command "show" (concat commit-id ":" filename-for-git)
                                     'keep-final-newline)))))

(defun helm-backup-create-backup-buffer (commit-id filename)
  "Create a buffer using chosen backup using COMMIT-ID and FILENAME."
  (helm-backup-in-backup-path
   (when-let ((data (helm-backup-fetch-backup-file commit-id filename)))
     (let ((buffer (get-buffer-create (concat filename " | " (helm-backup-exec-git-command
                                                              "diff-tree"
                                                              "-s"
                                                              "--pretty=format:%cd"
                                                              commit-id))))
           (mode major-mode))
       (with-current-buffer buffer
         (erase-buffer)
         (insert data)
         (funcall mode)
         (set-buffer-modified-p nil)
         buffer)))))

(defun helm-backup-clean-repository (&optional aggressive)
  "Clean repository running gc."
  (interactive (list t))
  (helm-backup-in-backup-path (apply #'helm-backup-exec-git-command
                                     "gc"
                                     (and aggressive '("--aggressive")))))

;;;###autoload
(defun helm-backup-versioning (&optional message force)
  "Helper to add easily versioning."
  (interactive (list (read-string "Commit message: ") t))
  (helm-backup-version-file (buffer-file-name) message force))

(defun helm-backup-open-in-new-buffer (commit-id filename)
  "Open backup in new buffer using COMMIT-ID and FILENAME."
  (let ((backup-buffer (helm-backup-create-backup-buffer commit-id filename)))
    (switch-to-buffer backup-buffer)))

(defun helm-backup-replace-current-buffer (commit-id filename)
  "Replace current buffer with backup using COMMIT-ID and FILENAME."
  (erase-buffer)
  (insert (helm-backup-fetch-backup-file commit-id filename)))

(defun helm-backup-create-ediff (commit-id buffer)
  "Create a ediff buffer with backup using COMMIT-ID and existing BUFFER."
  (let ((backup-buffer (helm-backup-create-backup-buffer commit-id (buffer-file-name buffer))))
    (ediff-buffers (buffer-name backup-buffer)
                   (buffer-name buffer))))


;;;; Helm interface

(defun helm-backup-source ()
  "Source used to populate buffer."
  `((name . ,(let ((file (buffer-file-name)))
               (format "Backup for %s under %s"
                       (file-name-nondirectory file)
                       (helm-backup-get-ref file))))
    (candidates . ,(helm-backup-list-file-change-time (buffer-file-name)))
    (action ("Ediff file with backup" .
             ,(lambda (candidate)
                (helm-backup-create-ediff candidate (current-buffer))))
            ("Open in new buffer" .
             ,(lambda (candidate)
                (helm-backup-open-in-new-buffer candidate (buffer-file-name))))
            ("Replace current buffer" .
             ,(lambda (candidate)
                (with-helm-current-buffer
                  (helm-backup-replace-current-buffer candidate (buffer-file-name))))))))

(defclass helm-backup-source-find-file-in-repo (helm-source)
  ((name :initform "Open backup repository")
   (candidates :initform (lambda () (list (file-name-nondirectory (buffer-file-name helm-current-buffer)))))
   (action :initform (("Open backup repository" .
                       (lambda (_cand) (dired-jump t (helm-backup-get-backup-file-name
                                                 (buffer-file-name helm-current-buffer))))))))
  "Open/ediff against revisions and index of current file.")

(defclass helm-backup-source-magit-log (helm-source)
  ((name :initform "Open Log with Magit")
   (candidates :initform '("Open Log with Magit"))
   (action :initform (("Open log with Magit" .
                       (lambda (_cand) (call-interactively #'magit-backup-magit-log))))))
  "Open/ediff against revisions and index of current file.")

;;;###autoload
(defun helm-backup ()
  "Main function used to call `helm-backup`."
  (interactive)
  (let ((helm-quit-if-no-candidate
         (lambda ()
           (error
            "No filename associated with buffer, file has no backup yet or filename is blacklisted"))))
    (helm-other-buffer (helm-backup-source) "*Helm Backup*")))


;;;; Magit log interface

(defvar magit-backup-magit-log-log-args '("--date-order" "--graph" "--decorate"))

;;;###autoload
(defun magit-backup-magit-log (&optional refs args)
  "Show backup log with Magit.
With region active, show only commits changing the region lines."
  (interactive
   (progn
     (require 'magit)
     (helm-backup-in-backup-path
      (list (list (helm-backup-get-ref (buffer-file-name)))))))
  (require 'magit)
  (let* ((file  (helm-backup-transform-filename-for-git (buffer-file-name)))
         (L-arg (and (region-active-p)
                     (format "-L%d,%d:%s"
                             (line-number-at-pos (region-beginning))
                             (line-number-at-pos (region-end))
                             file))))
    (helm-backup-in-backup-path
     (magit-log
      refs
      (or args (if L-arg (apply #'list  L-arg "-p" magit-backup-magit-log-log-args)
                 magit-backup-magit-log-log-args))
      (list file)))))


(helm-backup-clean-repository)

(provide 'helm-backup)



;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-backup.el ends here
