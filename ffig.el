;;; ffig.el --- Find files in a git repository quickly.

;; Copyright (C) 2009 Peter Teichman

(defun ffig (prompt-for-repo)
  "Find a file in the same repository as the current buffer's file.

With a prefix argument, prompt for the git repository to search."
  (interactive "P")
  (let* ((repo-file-name (if prompt-for-repo
                             (expand-file-name (read-file-name "Git repository: "))
                           (buffer-file-name)))
         (repo-files (ffig-repo-files (ffig-file-maybe-directory repo-file-name)))
         (file (completing-read "Find repo file: "
                     (mapcar 'car repo-files))))
    (find-file (cdr (assoc file repo-files)))))

(defun ffig-git-repository (file)
  "Find the git repository associated with FILE"
  (let ((git-dir ".git"))
    (expand-file-name
     (concat (locate-dominating-file file git-dir) git-dir))))

(defun ffig-ls-files (repo)
  "List the files in a git repository"
  (let ((toplevel (file-name-directory repo)))
    (mapcar (lambda(file) (concat toplevel file))
            (split-string (shell-command-to-string
                           (format "git --git-dir=%s ls-files -z" repo))
                          "\0" t))))

(defun ffig-filenames (files)
  "Return an alist of (filename . file-path) from a list of file-paths."
  (mapcar (lambda(file) (let ((parts (split-string file "/")))
                          (cons (car (last parts)) file)))
          files))

(defun ffig-repo-files (file)
  "Create an alist of files in the git repository for FILE.

Each element looks like:
(unique-filename . path-to-file)"
  (ffig-filenames (ffig-ls-files (ffig-git-repository file))))

(defun ffig-file-maybe-directory (file)
  (if (file-directory-p file)
      (file-name-as-directory file)
    file))

(provide 'ffig)
