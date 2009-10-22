;;; ffig.el --- Find files in a git repository quickly.

;; Copyright (C) 2009 Peter Teichman

(defun ffig (prompt-for-repo)
  "Find a file in the same repository as the current buffer's file.

With a prefix argument, prompt for the git repository to search."
  (interactive "P")
  (let* ((repo-path (ffig-get-default-repository prompt-for-repo))
         (repo-files (ffig-repo-files (ffig-file-maybe-directory repo-path)))
         (read (if (and (boundp 'ido-mode) ido-mode)
                   'ido-completing-read
                 'completing-read))
         (file (funcall read "Find repo file: " (mapcar 'car repo-files))))
    (find-file (cdr (assoc file repo-files)))))

(defun ffig-grep (prompt-for-repo)
  "Run M-x grep over a git repository.

With a prefix argument, prompt for the git repository to search."
  (interactive "P")
  (let* ((repo-path (ffig-get-default-repository prompt-for-repo))
         (command (read-shell-command
                   "Run git-grep (like this): "
                   (format "git --no-pager --git-dir=%s --work-tree=%s grep -n "
                           repo-path
                           (file-name-directory repo-path)))))
    (grep command)))

(defun ffig-get-default-repository (prompt-for-repo)
  (let ((default-repo (ffig-git-repository (buffer-file-name))))
    (if (or prompt-for-repo (not default-repo))
        (ffig-git-repository (read-file-name "Git repository: "))
      default-repo)))

(defun ffig-git-repository (file)
  "Find the git repository associated with FILE"
  (when file
    (let ((git-dir ".git"))
      (expand-file-name
       (concat (ffig-locate-dominating-file
                (ffig-file-maybe-directory file) git-dir)
               git-dir)))))

(defun ffig-ls-files (repo)
  "List the files in a git repository"
  (let ((toplevel (file-name-directory repo)))
    (mapcar (lambda(file) (concat toplevel file))
            (split-string (shell-command-to-string
                           (format "git --git-dir=%s ls-files -z" repo))
                          "\0" t))))

(defun ffig-filenames (files)
  "Return an alist of (filename . file-path) from a list of file-paths."
  (mapcar (lambda(file) (cons (file-name-nondirectory file) file))
          files))

(defun ffig-split-paths (paths)
  (mapcar (lambda(path)
            (reverse (split-string path "/" t)))
          paths))

(defun ffig-uniquify-file (file)
  "(basename file1 file2 file3)
    => ((basename<file1> . file1) (basename<file2> . file2))"
  (if (caddr file)
      (let* ((paths (cdr file))
             (split-paths (ffig-remove-common-prefix (ffig-split-paths paths)))
             (ret nil))
        (while paths
          (add-to-list 'ret (cons (format "%s<%s>" (car file) (caar split-paths)) (car paths)))
          (setq paths (cdr paths))
          (setq split-paths (cdr split-paths)))
        ret)
    (list (cons (car file) (cadr file)))))

(defun ffig-uniquify (files)
  "((basename . file1) (basename . file2))
    => ((basename<file1> . file1) (basename<file2> . file2))"
  (let ((alist (ffig-collapse-alist files))
        (ret nil))
    (mapcar (lambda (cur)
              (mapcar (lambda(file)
                        (add-to-list 'ret file))
                      (ffig-uniquify-file cur)))
            alist)
    ret))

(defun ffig-collapse-alist (alist)
"((basename . file1) (basename . file2)) => (basename file file2)"
  (let ((cur alist)
        (hash (make-hash-table :test 'equal))
        (ret nil))
    (while cur
      (let* ((elt (car cur))
             (basename (car elt))
             (filename (cdr elt)))
        (puthash basename (cons filename (gethash basename hash '())) hash))
      (setq cur (cdr cur)))
    ; put the alist back together
    (maphash (lambda (key value)
               (setq ret (cons (append (list key) value) ret))) hash)
    ret))

(defun ffig-cars-equal (lists)
  (let ((cars (mapcar 'car lists))
        (ret t))
    (while (cdr cars)
      (if (not (equal (car cars) (cadr cars)))
          (setq ret nil))
      (setq cars (cdr cars)))
    ret))

(defun ffig-remove-common-prefix (lists)
  (let ((ret lists))
    (while (ffig-cars-equal ret)
      (setq ret (mapcar 'cdr ret)))
    ret))

(defun ffig-repo-files (file)
  "Create an alist of files in the git repository for FILE.

Each element looks like:
(unique-filename . path-to-file)"
  (ffig-uniquify (ffig-filenames (ffig-ls-files (ffig-git-repository file)))))

(defun ffig-file-maybe-directory (file)
  (if (file-directory-p file)
      (file-name-as-directory file)
    file))

; backport locate-dominating-file to emacs pre-23
(if (functionp 'locate-dominating-file)
    (defalias 'ffig-locate-dominating-file 'locate-dominating-file)
  (defun ffig-locate-dominating-file (file name)
    "Look up the dominating NAME in and above FILE."
    (let ((parent (file-truename (expand-file-name ".." file))))
      (cond ((string= file parent) nil)
            ((file-exists-p (concat file name)) file)
            (t (ffig-locate-dominating-file parent name))))))

(provide 'ffig)
