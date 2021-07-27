;;; radien-directories.el -*- lexical-binding: t -*-


;; BEAUTIFUL set of functions from Radian for creating directories when
;; finding files.

;; The following code is taken directly from Radian
;; https://github.com/raxod502/radian

(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file          ; C-x C-w
               ))
  (advice-add fun :around #'radian--advice-find-file-create-directories))


(provide 'radian-directories)
;;; radien-directories.el ends here
