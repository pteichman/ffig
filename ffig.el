;;; ffig.el --- Find files in a git repository quickly.

;; Copyright (C) 2013 Peter Teichman
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Peter Teichman <peter@teichman.org>
;; Version: 1.2.2
;; Keywords: files

;;; Commentary:
;;;
;;; (require 'ffig)
;;;
;;; This gives you an interactive command, M-x ffig. It works like M-x
;;; find-file, but autocompletes to all the files in your current git
;;; repository.
;;;
;;; Prefix it with C-u if you want to find a file in a different git
;;; repository.
;;;
;;; Identical filenames are made unique by appending the deepest unique
;;; portion of their path.
;;;
;;; Recommended keybinding:
;;;   (global-set-key (kbd "C-x C-M-f") 'ffig)

;;; Code:

(defgroup ffig nil
  "Find files in a git repository quickly."
  :group 'files)

(defcustom ffig-git-path "git"
  "Path to your git executable. This doesn't need customization if
it's in your PATH."
  :group 'ffig
  :type 'string)

;;;###autoload
(defun ffig (prompt-for-repo)
  "Find a file in the same repository as the current buffer's file.

With a prefix argument, prompt for the git repository to search."
  (interactive "P")
  (let* ((repo-path (ffig-get-default-repository prompt-for-repo))
         (repo-files (ffig-repo-files (ffig-file-maybe-directory repo-path)))
         (read (if (and (boundp 'ido-mode) ido-mode)
                   'ido-completing-read
                 'completing-read))
         (file (funcall read "Find repo file: " (mapc 'car repo-files)))
         (path (cdr (assoc file repo-files))))
    (if path
        (find-file path)
      (message "File not found in git repository: %s" file))))

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
                (ffig-file-maybe-directory file) git-dir) "/"
               git-dir)))))

(defun ffig-ls-files (repo)
  "List the files in a git repository"
  (let ((toplevel (file-name-directory repo)))
    (mapcar (lambda(file) (concat toplevel file))
            (split-string (shell-command-to-string
                           (format "%s --git-dir=%s ls-files -z"
                                   ffig-git-path repo))
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
  (if (cddr file)
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
    (mapc (lambda (cur)
              (mapc (lambda(file)
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
        (let* ((filelist (gethash basename hash '())))
          (add-to-list 'filelist filename)
          (puthash basename filelist hash))
      (setq cur (cdr cur))))
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
            ((file-exists-p (concat file "/" name)) file)
            (t (ffig-locate-dominating-file parent name))))))

(provide 'ffig)

;;; ffig.el ends here
