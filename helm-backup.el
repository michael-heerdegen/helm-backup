;;; helm-backup.el --- Backup each file change using git -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/helm-backup
;; Version: 0.3.0
;; Package-Requires: ((helm "1.5.5") (s "1.8.0") (cl-lib "0"))
;; Keywords: backup, convenience, files, tools, vc

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

;;; Commentary: (FIXME: update!)

;; To store change every time you save a file add :
;; (add-hook 'after-save-hook 'helm-backup-versioning)
;; or from Emacs you can do :
;; M-x customize-variable RET after-save-hook RET [INS] helm-backup-versioning

;; To retrieve file backup, from buffer call `helm-backup' :
;; M-x helm-backup
;; for convenience you can define key binding as follow :
;; (global-set-key (kbd "C-c b") 'helm-backup)
;;
;;
;; To do:
;;
;; - Can/should we indicate the time since the last backup?
;;
;; - Provide a command to allow deletion of files from history; allow
;; to forget early history (and optionally keep explicit named saves).
;; I guess this could be done with checking out the most recent
;; backup, rebase, and updating the backup ref afterwards.
;;
;; - Make it possible to backup complete directories?  Or the complete
;; repo, if in one?
;;
;; - Make it work with remote files/tramp
;;
;; - split into two files: the helm interface, and the backup mechanism


;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'cl-lib)
(require 'vc)
(require 'vc-git)
(require 's)

(defgroup helm-backup nil
  "Backup system using git and helm."
  :group 'helm)

(defcustom helm-backup-path "~/.helm-backup"
  "Backup location."
  :group 'helm-backup
  :set (lambda (symbol path) (set-default symbol (expand-file-name path)))
  :type 'string)

(defvar helm-backup-use-hardlinks t)

(defvar helm-backup-namespace "refs/backup/")

(defvar helm-backup-ref "all")

(defvar helm-backup--this-file nil)

(defcustom helm-backup-git-binary "git"
  "Git binary path."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-list-format "%h %cd: %s  (%ar)"
  "Format use to display entries in helm buffer, follow git log format."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-excluded-entries '()
  "Define a list of file/folder regexp to exclude from backup.
/home/user/password => exclude password in /home/user
.*\\.el$ => exclude .el extension
/root/.* => exclude everything inside root
.*/password/.* => exclude all folders with name 'password'"
  :group 'helm-backup
  :type '(repeat regexp))

(defun helm-backup-read-revision (prompt file &rest more-compl-read-args)
  ;; For null input return "HEAD"
  ;; (when (bound-and-true-p helm-mode)
  ;;   (when-let ((branch (helm-backup-get-branch-name file)))
  ;;     (run-with-idle-timer 0 nil `(lambda () (helm-preselect (rx-to-string `(and bol ,',branch eol)))))))
  (let ((helm-candidate-number-limit 9999)
        (icomplete-mode nil)
        (completion-table-from-vc
         (vc-call-backend 'Git 'revision-completion-table (list file)))
        (completion-table-of-ancestors
         (let ((cands ()))
           (letrec
               ((table
                 (lazy-completion-table
                  table (lambda () (cl-callf or cands
                                (append (helm-backup-list-revision-log
                                         file "HEAD" (list "--pretty=format:%ar %s"))
                                        (helm-backup-list-revision-log
                                         file
                                         (helm-backup-get-ref file)
                                         (list "--pretty=format:%ar %s"))
                                        (helm-backup-list-revision-log
                                         file "--all" (list "--pretty=format:%ar %s") t)))
                          (lambda (string pred action)
                            (complete-with-action
                             action
                             (mapcar (lambda (cand) (concat (cdr cand) " " (car cand))) cands)
                             string pred))))))
             (lambda (string pred action)
               (if (eq action 'metadata)
                   `(metadata (annotation-function . ,(lambda (s) (when-let ((s (car (rassoc s cands))))
                                                               (concat " " s))))
                              (display-sort-function . ,#'identity))
                 (complete-with-action action table string pred)))))))
    (let ((choice (apply #'completing-read
                         (concat prompt (format "(default %s) " (or (nth 6 more-compl-read-args)
                                                                    "HEAD")))
                         (completion-table-merge completion-table-from-vc completion-table-of-ancestors)
                         more-compl-read-args)))
      (cond
       ((string-match "\\`\\([0-9a-f]+\\) " choice)
        (match-string 1 choice))
       ((string= choice "")
        "HEAD")
       (t choice)))))

(defun helm-backup-exec-git-command (command &rest args)
  ;; FIXME: make additional args come before COMMAND, which is always a string. (?)
  "Execute git COMMAND and return result string, nil if failed."
  (let ((keep-final-newline nil)
        (should-error nil))
    (when (memq :keep-final-newline args)
      (setq keep-final-newline t
            args (delq :keep-final-newline args)))
    (when (memq :should-error args)
      (setq should-error t
            args (delq :should-error args)))
    (let ((output (with-temp-buffer
                    (let ((result (apply #'process-file helm-backup-git-binary
                                         nil '(t t) nil
                                         "--literal-pathspecs"
                                         "--no-pager"
                                         "-c" "core.preloadindex=true"
                                         command args)))
                      (cond
                       ((zerop result) (buffer-string))
                       (should-error   (error "Running git %s %s gave an error:
%s

Please make a bug report."
                                              command
                                              (mapconcat #'identity args " ")
                                              (buffer-string)))
                       (t nil))))))
      (when output
        (if keep-final-newline
            output
          (s-chomp output))))))

(defun helm-backup--repository-root (&optional file)
  (cl-callf or file (file-truename buffer-file-name))
  (let ((default-directory (if file (file-name-directory file) default-directory)))
    ;; when repo is empty, return nil, because if we do the first
    ;; commit, git automatically attaches master to it.
    (and
     (helm-backup-exec-git-command "rev-parse" "--verify" "HEAD")
     (vc-git-root file))))

(defun helm-backup-get-backup-path (&optional file)
  (or (helm-backup--repository-root file) helm-backup-path))

(defmacro helm-backup-in-backup-path (&rest body)
  (declare (debug t))
  `(let ((default-directory (helm-backup-get-backup-path helm-backup--this-file)))
     ,@body))

(defun helm-backup-git-command-return-value (command &rest args)
  (helm-backup-in-backup-path
    (apply #'process-file helm-backup-git-binary nil nil nil command args)))

(defun helm-backup-get-ref (file)
  (cl-callf file-truename file)
  (let ((default-directory (file-name-directory file)))
    (concat helm-backup-namespace helm-backup-ref)))

(defun helm-backup-get-branch-name (file)
  (let ((default-directory (vc-git-root file)))
    (cl-callf file-truename file))
  (let ((default-directory (file-name-directory file)))
    (and (helm-backup--repository-root file)
         (when-let ((file (helm-backup-exec-git-command "symbolic-ref" "--quiet" "HEAD")))
           (file-name-nondirectory file)))))

(defun helm-backup-verify-ref (ref)
  (helm-backup-in-backup-path
   (helm-backup-exec-git-command "rev-parse" "--quiet" "--verify" ref)))

(defun helm-backup-init-git-repository ()
  "Initialize git repository."
  (when (and ;; (file-equal-p helm-backup-path (helm-backup-get-backup-path))
             (not (file-exists-p helm-backup-path)))
    (helm-backup-exec-git-command "init" :should-error helm-backup-path)
    (helm-backup-in-backup-path
     (helm-backup-exec-git-command "config" "--local" "user.email" "noemail@noemail.com")
     (helm-backup-exec-git-command "config" "--local" "user.name" "noname"))))

(defun helm-backup-transform-filename-for-git (filename)
  "Transform FILENAME to be used in git repository."
  (let ((root (helm-backup--repository-root filename)))
    (if root (file-relative-name filename root)
      (file-relative-name filename "/"))))

(defun helm-backup-copy-file-to-repository (filename)
  "Create folder in repository and copy file using FILENAME in it."
  (cl-callf file-truename filename)
  (unless (helm-backup--repository-root filename)
    (let ((directory (expand-file-name (file-name-directory
                                        (helm-backup-transform-filename-for-git filename))
                                       helm-backup-path)))
      (make-directory directory t)
      (when (or (not helm-backup-use-hardlinks)
                (file-remote-p filename)
                (not (ignore-errors
                       ;; this can fail due to trying to hardlinking to a different device
                       (add-name-to-file
                        filename
                        (expand-file-name (file-name-nondirectory filename) directory) t)
                       t)))
        (condition-case nil
            (copy-file filename directory t t t)
          (file-error))))))

(defun helm-backup-get-backup-file-name (filename)
  (cl-callf file-truename filename)
  (if (helm-backup--repository-root filename)
      filename
    (expand-file-name (file-name-nondirectory filename)
                      (concat helm-backup-path (file-name-directory filename)))))

(defun helm-backup-file-excluded-p (filename)
  "Check if a FILENAME is excluded from backup."
  (cl-some (lambda (regexp) (string-match-p (concat "\\`" regexp "\\'") filename))
           (cons "\\(.*/\\)?\\.git/.*" helm-backup-excluded-entries)))

(defun helm-backup--reset-index (&optional tree-ish)
  ;; Note: "reset tree-ish --" changes HEAD, so we can't use it here
  (helm-backup-exec-git-command "reset" :should-error)
  (when tree-ish (helm-backup-exec-git-command "read-tree" :should-error tree-ish)))

(defmacro helm-backup-save-index (&rest body)
  ;; Modeled after `magit-with-temp-index'
  (declare (debug t))
  (macroexp-let2 nil temp-index-file '(expand-file-name (make-temp-name "index.helm-backup"))
    `(unwind-protect
         (let ((process-environment (cons (concat "GIT_INDEX_FILE=" ,temp-index-file)
                                          process-environment)))
           ,@body)
       (when (file-exists-p ,temp-index-file) (delete-file ,temp-index-file)))))

(defvar-local helm-backup-time-of-latest-backup nil
  "Time of latest backup of this buffer's file in this Emacs session.
Format is in seconds since epoch.")

(defun helm-backup-version-file (filename &optional message force)
  "Version file using FILENAME in backup repository.
Return non-nil when a commit has been made."
  (cl-callf file-truename filename) ;git can't follow symlinks
  (cl-callf or message "backup")
  (when (and filename
             (file-name-absolute-p filename)
             (file-exists-p filename)
             (not (helm-backup-file-excluded-p filename)))
    (helm-backup-init-git-repository)
    (helm-backup-copy-file-to-repository filename)
    (prog1
        (let ((ref (helm-backup-get-ref filename)))
          (helm-backup-in-backup-path
           (helm-backup-save-index
            (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
              (unless (helm-backup-verify-ref ref)
                ;; reset index and make initial void commit under ref
                (helm-backup-exec-git-command "rm" "--cached" "*")
                (helm-backup-exec-git-command
                 "update-ref" ref "--create-reflog" :should-error
                 (helm-backup-exec-git-command
                  "commit-tree" :should-error
                  "-m" (format "starting backing up in %s" ref)
                  (helm-backup-exec-git-command "write-tree" :should-error))))
              ;; Prepare index:  Reset index to appropriate commit and add this file.
              (helm-backup--reset-index ref)
              (helm-backup-exec-git-command "add" "-f" filename-for-git :should-error)
              (when (or force
                        (= 1 (helm-backup-git-command-return-value
                              "diff" "--quiet"
                              ref "--" filename-for-git)))
                (let ((parent (if-let ((git-call (and force
                                                      (not (= 1 (helm-backup-git-command-return-value
                                                                 "diff" "--quiet"
                                                                 ref "--" filename-for-git)))
                                                      (helm-backup-exec-git-command
                                                       "log" ref
                                                       "-2"
                                                       "--pretty=format:%H"
                                                       filename-for-git)))
                                       (second (nth 1 (split-string git-call "\n"))))
                                  second
                                ref)))
                  (helm-backup-exec-git-command
                   "update-ref" ref "--create-reflog" :should-error
                   (helm-backup-exec-git-command
                    "commit-tree" :should-error
                    "-m" message
                    "-p" parent
                    (helm-backup-exec-git-command "write-tree" :should-error))))
                t)))
           ;; $$$$$FIXME: is this kosher???  Make it customizable (yes,
           ;; no, only for global repo) Make it so that we have a variable
           ;; bound to non-nil when and only when a gc process is running.
           ;; Delay any other functions trying to modify the repo.
           ;; (call-process "git" nil nil nil "gc")
           ))
     (when-let ((file-buffer (get-file-buffer filename)))
       (with-current-buffer file-buffer
         (setq-local helm-backup-time-of-latest-backup
                     (helm-backup-get-age-of-last-backup buffer-file-name)))))))

;; (defvar-local helm-backup--async-process-running-p nil)

;; (defun helm-backup-version-file-async--start-function (filename &optional message force)
;;   (let ((helm-backup (locate-library "helm-backup")))
;;     (lambda ()
;;       (load helm-backup)
;;       (helm-backup-version-file filename message force))))

;; (defun helm-backup-version-file-async--finish (result)
;;   (setq helm-backup--async-process-running-p nil)
;;   (message "%s" result))

;; (defun helm-backup-version-file-async (filename &optional message force)
;;   (setq helm-backup--async-process-running-p t)
;;   (async-start (helm-backup-version-file-async--start-function filename message force)
;;                #'helm-backup-version-file-async--finish))

(defvar-local helm-backup--not-first-save-p nil)

(defun helm-backup-before-save ()
  "Backup before save."
  (when (helm-backup-versioning
         (concat (let ((branch (helm-backup-get-branch-name buffer-file-name)))
                   (when branch (concat "[" branch "] ")))
                 "before save"
                 (unless helm-backup--not-first-save-p
                   " (new session)")))
    (setq helm-backup--not-first-save-p t)))

(defun helm-backup-get-age-of-last-backup (filename)
  "Doc.."
  (cl-callf file-truename filename)
  (let ((ref (helm-backup-get-ref filename)))
    (when (helm-backup-verify-ref ref)
      (helm-backup-in-backup-path
       (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
         (ignore-errors
           (string-to-number
            ;; Don't use date=%s since it's unreliable (bug in git)
            (helm-backup-exec-git-command "log" ref "--pretty=format:%ct" "-n1"
                                          filename-for-git))))))))

(defun helm-backup-after-save ()
  "Backup after save."
  (when (helm-backup-versioning
         (concat (let ((branch (helm-backup-get-branch-name buffer-file-name)))
                   (when branch (concat "[" branch "] ")))
                 "after save"
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

(defun helm-backup-list-revision-log (filename ref git-format-arg-list &optional dont-verify)
  (cl-callf file-truename filename)
  (when (or dont-verify (helm-backup-verify-ref ref))
    (helm-backup-in-backup-path
     (let* ((filename-for-git (helm-backup-transform-filename-for-git filename))
            (hashes (helm-backup-exec-git-command
                     "log" "--pretty=format:%h"
                     "--remove-empty" "--no-renames" ;FIXME: doesn't have an effect for me (git 2.8.1)
                     ref "--" filename-for-git))
            (formatted (apply #'helm-backup-exec-git-command
                              `("log" ,ref ,@git-format-arg-list ,filename-for-git))))
       (when hashes (cl-mapcar #'cons
                               (split-string formatted "\n")
                               (split-string hashes    "\n")))))))

(defun helm-backup-list-file-change-time (filename)
  "Build assoc list using commit id and message rendering format using FILENAME."
  (cl-callf file-truename filename)
  (helm-backup-list-revision-log
   filename
   (helm-backup-get-ref filename)
   (list (format "--pretty=format:%s" helm-backup-list-format) "--date=local")))

(defun helm-backup-create-revision-buffer (revision filename)
  "Create a buffer using chosen REVISION and FILENAME.
This works for backup revisions as well for normal revisions."
  ;; It works for normal revisions because `helm-backup-path' is only
  ;; looked in if we are not in a repository anyway
  (cl-callf file-truename filename)
  (helm-backup-in-backup-path
   (let* ((filename-for-git (helm-backup-transform-filename-for-git filename))
          (temp-file (vc-version-backup-file-name filename revision 'manual)))
     (when (and revision
                (not (string= (helm-backup-exec-git-command "log"
                                                            :should-error
                                                            "--ignore-missing" "-1"
                                                            revision "--" filename-for-git)
                              "")))
       (let ((coding-system-for-read 'no-conversion)
             (coding-system-for-write 'no-conversion))
         (with-temp-file temp-file
           (vc-call-backend 'Git 'find-revision filename-for-git revision (current-buffer))))
       (let* ((auto-mode-alist `((".*" . ,(with-demoted-errors
                                              (with-current-buffer (find-file-noselect filename)
                                                major-mode)))))
              (result-buf (find-file-noselect temp-file)))
         (with-current-buffer result-buf
           (delete-file temp-file)
           (let ((change-major-mode-with-file-name nil))
             (set-visited-file-name nil))
           (current-buffer)))))))

(defun helm-backup-clean-repository (&optional aggressive)
  "Clean repository running gc."
  (interactive (list t))
  (when (file-exists-p helm-backup-path)
    (let ((default-directory helm-backup-path))
      (apply #'helm-backup-exec-git-command
             "gc" :should-error
             (and aggressive '("--aggressive"))))))

;;;###autoload
(defun helm-backup-versioning (&optional message force)
  "Helper to add easily versioning."
  (interactive (list (let* ((message (read-string "Commit message: " nil nil
                                                 (format-time-string "%b-%d %H:%M" (current-time))))
                            (branch (helm-backup-get-branch-name (file-truename buffer-file-name))))
                       (if branch
                           (concat "[" branch "] " message)
                         message))
                     t))
  (helm-backup-version-file (file-truename (buffer-file-name)) message force))

(defun helm-backup-open-in-new-buffer (revision filename)
  "Open backup in new buffer using REVISION and FILENAME."
  (pop-to-buffer-same-window (helm-backup-create-revision-buffer revision (file-truename filename))))

(defun helm-backup-replace-current-buffer (revision filename)
  "Replace current buffer with backup using REVISION and FILENAME."
  (cl-callf file-truename filename)
  (erase-buffer)
  (insert
   (helm-backup-in-backup-path
    (let ((backup-buffer (helm-backup-create-revision-buffer revision filename)))
      (prog1 (with-current-buffer backup-buffer
               (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer backup-buffer))))))

(defun helm-backup-create-ediff (revision buffer)
  "Create a ediff buffer with backup using REVISION and existing BUFFER."
  (let ((backup-buffer (helm-backup-create-revision-buffer
                        revision (file-truename (buffer-file-name buffer)))))
    (ediff-buffers (buffer-name backup-buffer)
                   (buffer-name buffer))))


;;;; Helm interface

(defun helm-backup-source () ;FIXME: make a var
  "Source used to populate buffer."
  `((name . ,(let ((file (buffer-file-name)))
               (format "Backup for %s under %s"
                       (file-name-nondirectory file)
                       (helm-backup-get-ref file))))
    (candidates . ,(helm-backup-list-file-change-time (buffer-file-name)))
    (candidate-transformer
     .
     (lambda (cands)
       (with-helm-current-buffer
         (let ((i 0))
           (mapcar (pcase-lambda ((and cand `(,descr . ,hash)))
                     (cl-incf i)
                     (cond
                      ;; ((string-match "before save" descr)
                      ;;  (cons (propertize descr
                      ;;                    'face 'warning)
                      ;;        hash))
                      ((and (< i 30)
                            (helm-backup-in-backup-path
                             (helm-backup-exec-git-command
                              "diff" "--quiet" hash
                              (helm-backup-transform-filename-for-git
                               (file-truename buffer-file-name)))))
                       (cons (propertize (concat descr " (identical with file)")
                                         'face 'success)
                             hash))
                      ((and
                        (< i 30)
                        ;; FIXME: When file is untracked, git-diff will report the file as not
                        ;; existent in the working dir
                        (vc-git-registered (file-truename buffer-file-name))
                        (eq cand (car cands)))
                       (cons (propertize (concat descr " (different from file)") 'face 'warning)
                             hash))
                      (t cand)))
                   cands)))))
    (action ("Ediff file with backup" .
             ,(lambda (candidate)
                (helm-backup-create-ediff candidate (current-buffer))))
            ("Open in new buffer" .
             ,(lambda (candidate)
                (helm-backup-open-in-new-buffer candidate (buffer-file-name))))
            ("Replace current buffer" .
             ,(lambda (candidate)
                (with-helm-current-buffer
                  (helm-backup-replace-current-buffer candidate (buffer-file-name)))))
            ("Push revision hash to kill-ring" .
             ,(lambda (candidate)
                (let ((hash (substring candidate 0 7)))
                  (kill-new hash)
                  (message "Added \"%s\" to kill ring" hash)))))))

(defclass helm-backup-source-find-file-in-repo (helm-source)
  ((name :initform "Open backup repository")
   (candidates :initform (lambda () (list (file-name-nondirectory
                                      (file-truename
                                       (buffer-file-name helm-current-buffer))))))
   (action :initform (("Open backup repository" .
                       (lambda (_cand) (dired-jump t (helm-backup-get-backup-file-name
                                                 (buffer-file-name helm-current-buffer))))))))
  "Open/ediff against revisions and index of current file.")

(defclass helm-backup-source-magit-log (helm-source)
  ((name :initform "Magit Log")
   (candidates :initform '("Show log of backups with Magit"))
   (action :initform
           (("log with Magit" .
             (lambda (_cand) (call-interactively #'helm-backup-magit-log)))
            ("log -p with Magit" .
             (lambda (_cand) (let ((current-prefix-arg 1))
                          (call-interactively #'helm-backup-magit-log))))))))

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

(defvar helm-backup-magit-log-log-args '("-p" "--date-order" "--graph" "--decorate"))

(defun helm-backup--L-arg (file-name)
  (and (region-active-p)
       (format "-L%d,%d:%s"
               (line-number-at-pos (region-beginning))
               (line-number-at-pos (save-excursion
                                     (goto-char (region-end))
                                     (when (eq (char-before) ?\n) (backward-char))
                                     (point)))
               file-name)))

;;;###autoload
(defun helm-backup-magit-log (&optional refs args)
  "Show backup log with Magit.
With region active, show only commits changing the region lines."
  (interactive
   (progn
     (require 'magit)
     (helm-backup-in-backup-path
      (let ((file (file-truename (buffer-file-name))))
        (list (list (helm-backup-get-ref file))
              (delq nil (list "-p" "-n25" (helm-backup--L-arg file))))))))
  (require 'magit)
  (let* ((file  (helm-backup-transform-filename-for-git (file-truename (buffer-file-name))))
         (L-arg (helm-backup--L-arg file)))
    (helm-backup-in-backup-path
     (magit-log
      refs
      (or args
          (if L-arg (apply #'list  L-arg helm-backup-magit-log-log-args)
            helm-backup-magit-log-log-args))
      (list file)))))

;;;###autoload
(defun helm-backup-file-from-dired (files &optional arg)
  (interactive (list (dired-get-marked-files) current-prefix-arg))
  (let* ((default-backup-name (format-time-string "%b-%d %H:%M" (current-time)))
         (backup-name (if arg (read-string "Backup name: " nil nil default-backup-name)
                        default-backup-name)))
    (dolist (file files)
      (cl-callf file-truename file)
      (when-let ((file-buffer (get-file-buffer file)))
        (when (and
               (buffer-modified-p file-buffer)
               (y-or-n-p "Save file buffer before backup?"))
          (with-current-buffer file-buffer (basic-save-buffer))))
      (let* ((helm-backup--this-file file))
        (cond
         ((and (> (nth 7 (file-attributes file)) (* 5 1000 1000)) ;5 MB
               (not (y-or-n-p (format "File %s is large - backup anyway? "
                                      (file-relative-name file)))))
          (progn
            (message "Skipped")
            (sit-for 1.)))
         ((helm-backup-version-file file
                                    (if-let ((branch (helm-backup-get-branch-name file)))
                                        (concat "[" branch "] " backup-name)
                                      backup-name))
          (unless (cdr files) (message "Backup created")))
         (t
          (unless (cdr files) (message "Backup already up to date"))))))))

(helm-backup-clean-repository)

(provide 'helm-backup)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-backup.el ends here
