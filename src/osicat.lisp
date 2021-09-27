;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; osicat.lisp --- High-level OS interface for non Windows systems
;;;
;;; Copyright (C) 2003, 2004 Nikodemus Siivola <nikodemus@random-state.net>
;;; Copyright (C) 2003, 2004 Julian Squires <jsquires@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:osicat)

;;;; Environment access
(defun (setf environment-variable) (value name)
  (nix:setenv (to-simple-string name) (to-simple-string value)))

(defun makunbound-environment-variable (name)
  "Removes the environment variable identified by NAME from the
current environment.  NAME can be either a string or a symbol.
Returns the string designated by NAME.  Signals an error on
failure."
  (nix:unsetenv (to-simple-string name)))

(defun (setf environment) (alist)
  (let ((oldenv (environment)))
    (loop for (var . val) in alist
          do (setf (environment-variable var) (string val)
                   oldenv (delete var oldenv
                                  :key (lambda (x) (string (car x)))
                                  :test #'string=)))
    (loop for (var . nil) in oldenv
          do (makunbound-environment-variable var)))
  alist)

;;;; Directory access
(defun call-with-directory-iterator (pathspec fun)
  (let ((dir (absolute-pathname (pathname pathspec)))
        (old-dir (current-directory)))
    (let ((dp (nix:opendir dir)))
      (labels ((one-iter ()
                 (let ((name (nix:readdir dp)))
                   (unless (null name)
                     (cond
                       ((member name '("." "..") :test #'string=)
                        (one-iter))
                       ((eq :directory (%get-file-kind name t))
                        (make-pathname :directory `(:relative ,name)))
                       (t
                        (let ((dotpos (position #\. name :from-end t)))
                          (if (and dotpos (plusp dotpos))
                              (make-pathname :name (subseq name 0 dotpos)
                                             :type (subseq name (1+ dotpos)))
                              (make-pathname :name name)))))))))
        (unwind-protect
             (let ((*default-pathname-defaults* dir))
               (setf (current-directory) dir)
               (funcall fun #'one-iter))
          (nix:closedir dp)
          (setf (current-directory) old-dir))))))

(defun mapdir (function pathspec)
  "Applies function to each entry in directory designated by
PATHSPEC in turn and returns a list of the results.  Binds
*DEFAULT-PATHNAME-DEFAULTS* to the directory designated by
pathspec round to function call.

If PATHSPEC designates a symbolic link, it is implicitly resolved.

Signals an error if PATHSPEC is wild or doesn't designate a directory."
  (with-directory-iterator (next pathspec)
    (loop for entry = (next)
          while entry
          collect (funcall function entry))))

(defun list-directory (pathspec &key bare-pathnames)
  "Returns a fresh list of pathnames corresponding to all files
within the directory named by the non-wild pathname designator
PATHSPEC.
If BARE-PATHNAMES is non-NIL only the files's bare pathnames are returned
\(with an empty directory component), otherwise the files' pathnames are
merged with PATHSPEC."
  (let ((pathspec (pathname-as-directory pathspec)))
    (with-directory-iterator (next pathspec)
      (loop for entry = (next)
            while entry collect (if bare-pathnames entry
                                    (merge-pathnames entry pathspec))))))

(defun walk-directory (dirname fn &key directories (if-does-not-exist :error)
                                    (test (constantly t)))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  Returns T on success.

FN will only be applied to files for which the function TEST
returns a true value.  If DIRECTORIES is not NIL, FN and TEST are
applied to directories as well.  If DIRECTORIES is :DEPTH-FIRST,
FN will be applied to the directory's contents first.  If
DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the
directory's content will be skipped. IF-DOES-NOT-EXIST must be
one of :ERROR or :IGNORE where :ERROR means that an error will be
signaled if the directory DIRNAME does not exist."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (mapdir #'walk name))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (mapdir #'walk name)))
                      ;; :DEPTH-FIRST is implicit
                      (t (mapdir #'walk name)
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        (:error
         (cond ((not (file-exists-p pathname-as-directory))
                (system-error "File ~S does not exist." pathname-as-directory))
               (t (walk pathname-as-directory) t)))
        (:ignore
         (if (file-exists-p pathname-as-directory)
             (progn (walk pathname-as-directory) t)
             nil))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))))

(defun delete-directory-and-files (dirname &key (if-does-not-exist :error))
  "Recursively deletes all files and directories within the directory
designated by the non-wild pathname designator DIRNAME including
DIRNAME itself.  IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE
where :ERROR means that an error will be signaled if the directory
DIRNAME does not exist."
  (walk-directory dirname
                  (lambda (file)
                    (cond ((directory-pathname-p file)
                           (delete-directory file))
                          (t (delete-file file))))
                  :directories t
                  :if-does-not-exist if-does-not-exist))

;;;; Symbolic and hard links

(defun read-link (pathspec)
  "Returns the pathname pointed to by the symbolic link
designated by PATHSPEC.  If the link is relative, then the
returned pathname is relative to the link, not
*DEFAULT-PATHNAME-DEFAULTS*.

Signals an error if PATHSPEC is wild, or does not designate a
symbolic link."
  ;; Note: the previous version tried much harder to provide a buffer
  ;; big enough to fit the link's name.  OTOH, NIX:READLINK stack
  ;; allocates on most lisps.
  (pathname (nix:readlink (absolute-pathname pathspec))))

(defun make-link (link &key target hard)
  "Creates LINK that points to TARGET.  Defaults to a symbolic
link, but giving a non-NIL value to the keyword argument :HARD
creates a hard link.  Returns the pathname of the link.

Relative targets are resolved against the link.  Relative links
are resolved against *DEFAULT-PATHNAME-DEFAULTS*.

Signals an error if either target or link is wild, target does
not exist, or link exists already."
  (unless target
    (error "No target given to MAKE-LINK."))
  (let ((old (current-directory)))
    (unwind-protect
         ;; KLUDGE: We merge against link for hard links only,
         ;; since symlink does the right thing once we are in
         ;; the correct directory.
         (progn
           (setf (current-directory)
                 (absolute-pathname *default-pathname-defaults*))
           (if hard
               (nix:link (merge-pathnames target link) link)
               (nix:symlink target link))
           (pathname link))
      (setf (current-directory) old))))

;;;; USER INFORMATION

(defun user-info (id)
  "USER-INFO returns the password entry for the given name or
numerical user ID, as an assoc-list."
  (multiple-value-bind (name password uid gid gecos home shell)
      (etypecase id
        (string (nix:getpwnam id))
        (integer (nix:getpwuid id)))
    (declare (ignore password))
    (unless (null name)
      (list (cons :name name)
            (cons :user-id uid)
            (cons :group-id gid)
            (cons :gecos gecos)
            (cons :home home)
            (cons :shell shell)))))
