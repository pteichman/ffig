;;; ffig.el --- Find files in a git repository quickly.

;; Copyright (C) 2009 Peter Teichman

(defun ffig ()
  "Find a file in the same repository as the current buffer's file"
  (interactive)
  (let* ((repo-files (ffig-repo-files (buffer-file-name)))
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
  (split-string (shell-command-to-string
                 (format "git --git-dir=%s ls-files -z" repo))
                "\0" t))

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

(provide 'ffig)
