;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; osicat.lisp --- High-level OS interface for all OSes
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

;;; FIXME: This is a *very* big kludge, waiting for babel to be fixed
(defun to-simple-string (thing)
  (let ((s (string thing)))
    (make-array (length s)
                :element-type 'character
                :initial-contents s)))

(defun environment-variable (name)
  "ENVIRONMENT-VARIABLE returns the environment variable
identified by NAME, or NIL if one does not exist.  NAME can
either be a symbol or a string.

SETF ENVIRONMENT-VARIABLE sets the environment variable
identified by NAME to VALUE.  Both NAME and VALUE can be either a
symbols or strings. Signals an error on failure."
  (nix:getenv (to-simple-string name)))

;;;; Environment access
(defun (setf environment-variable) (value name)
  #+windows
  (progn
    (unless (null (position #\= (to-simple-string name)))
      (error "Variable names cannot contain an #\\= character."))
    (nix:putenv (to-simple-string (format nil "~A=~A" name value))))
  #-windows
  (nix:setenv (to-simple-string name) (to-simple-string value)))

(defun makunbound-environment-variable (name)
  "Removes the environment variable identified by NAME from the
current environment.  NAME can be either a string or a symbol.
Returns the string designated by NAME.  Signals an error on
failure."
  #+windows
  (nix:putenv (to-simple-string (format nil "~A=" name)))
  #-windows
  (nix:unsetenv (to-simple-string name)))

(defun environment ()
  "ENVIRONMENT returns the current environment as an assoc-list.
SETF ENVIRONMENT modifies the environment its argument.

Often it is preferable to use SETF ENVIRONMENT-VARIABLE and
MAKUNBOUND-ENVIRONMENT-VARIABLE to modify the environment instead
of SETF ENVIRONMENT."
  (handler-case
      (loop for i from 0 by 1
            for string = (mem-aref nix:*environ* :string i)
            for split = (position #\= string)
            while string
            collecting (cons (subseq string 0 split)
                             (subseq string (1+ split))))
    #-(and)
    (error (e)
      (error "Could not access environment (~S)." e))))

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

;;;; Common subroutines

;;; FIXME: make sure that GET-FILE-KIND be able to signal
;;;        only conditions of type FILE-ERROR, either by
;;;        wrapping POSIX-ERRORs or making sure that some
;;;        POSIX-ERRORS subclass FILE-ERROR
(defun %get-file-kind (namestring follow-p)
  (handler-case
      (let ((mode (nix:stat-mode
                   (if follow-p
                       (nix:stat namestring)
                       #-windows
                       (nix:lstat namestring)))))
        (case (logand nix:s-ifmt mode)
          (#.nix:s-ifdir  :directory)
          (#.nix:s-ifchr  :character-device)
          (#.nix:s-ifblk  :block-device)
          (#.nix:s-ifreg  :regular-file)
          #-windows ; KLUDGE
          (#.nix:s-iflnk  :symbolic-link)
          #-windows ; KLUDGE
          (#.nix:s-ifsock :socket)
          (#.nix:s-ififo  :pipe)
          (otherwise
           (bug "Unknown file mode: ~A." mode))))
    (nix:enoent ()
      (cond
        ;; stat() returned ENOENT: either FILE does not exist
        ;; or the end of the symlink chain is a broken symlink
        #-windows
        (follow-p
         (handler-case
             (nix:lstat namestring)
           (nix:enoent ())
           (:no-error (stat)
             (declare (ignore stat))
             (values :symbolic-link :broken))))
        ;; lstat() returned ENOENT: FILE does not exist
        (t nil)))))

(defun get-file-kind (file follow-p)
  (%get-file-kind (native-namestring file) follow-p))

;;;; Hopefully portable pathname manipulations

(defun absolute-pathname-p (pathspec)
  "Returns T if the PATHSPEC designates an absolute pathname, NIL otherwise."
  (eq :absolute (car (pathname-directory pathspec))))

(defun relative-pathname-p (pathspec)
  "Returns T if the PATHSPEC designates a relative pathname, NIL otherwise."
  (not (absolute-pathname-p pathspec)))

(defun absolute-pathname (pathspec
                          &optional (default *default-pathname-defaults*))
  "Returns an absolute pathname corresponding to PATHSPEC by
merging it with DEFAULT, and (CURRENT-DIRECTORY) if necessary."
  (if (relative-pathname-p pathspec)
      (let ((tmp (merge-pathnames
                  pathspec
                  (make-pathname :name nil :type nil :version nil
                                 :defaults default))))
        (if (relative-pathname-p tmp)
            (merge-pathnames tmp (current-directory))
            tmp))
      pathspec))

(defun unmerge-pathnames (pathspec
                          &optional (default *default-pathname-defaults*))
  "Removes those leading directory components from PATHSPEC that
are shared with DEFAULT."
  (let* ((dir (pathname-directory pathspec))
         (mismatch (mismatch dir (pathname-directory default) :test #'equal)))
    (make-pathname :directory (when mismatch
                                `(:relative ,@(subseq dir mismatch)))
                   :defaults pathspec)))

;;; Helper function for DIRECTORY-PATHNAME-P which checks whether
;;; VALUE is neither NIL nor the keyword :UNSPECIFIC.
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC (a pathname designator) does not
designate a directory, PATHSPEC otherwise.  It is irrelevant
whether file or directory designated by PATHSPEC does actually
exist."
  (and (not (component-present-p (pathname-name pathspec)))
       (not (component-present-p (pathname-type pathspec)))
       pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to
directory form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (system-error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p pathspec))
        (make-pathname :directory (append (or (pathname-directory pathname)
                                              (list :relative))
                                          (list (file-namestring pathname)))
                       :name nil
                       :type nil
                       :defaults pathname)
        pathname)))

(defun pathname-directory-pathname (pathspec)
  "Returns the directory part of PATHSPEC as a pathname."
  (let ((pathname (pathname pathspec)))
    (make-pathname :name nil :type nil :defaults pathname)))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (system-error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p pathspec)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname :directory (butlast directory)
                         :name (pathname-name name-and-type)
                         :type (pathname-type name-and-type)
                         :defaults pathname))
        pathname)))

;;;; FILE-KIND

(defun file-kind (pathspec &key follow-symlinks)
  "Returns a keyword indicating the kind of file designated by PATHSPEC,
or NIL if the file does not exist.  Does not follow symbolic
links by default.

Possible file-kinds in addition to NIL are: :REGULAR-FILE,
:SYMBOLIC-LINK, :DIRECTORY, :PIPE, :SOCKET, :CHARACTER-DEVICE, and
:BLOCK-DEVICE.
If FOLLOW-SYMLINKS is non-NIL and PATHSPEC designates a broken symlink
returns :BROKEN as second value.

Signals an error if PATHSPEC is wild."
  (get-file-kind (merge-pathnames pathspec) follow-symlinks))

(defun file-exists-p (pathspec &optional file-kind)
  "Checks whether the file named by the pathname designator
PATHSPEC exists, if this is the case and FILE-KIND is specified
it also checks the file kind. If the tests succeed, return two values:
truename and file kind of PATHSPEC, NIL otherwise.
Follows symbolic links."
  (let* ((follow (unless (eq file-kind :symbolic-link) t))
         (actual-kind (file-kind pathspec :follow-symlinks follow)))
    (when (and actual-kind
               (if file-kind (eq file-kind actual-kind) t))
      (values (truename pathspec)
              actual-kind))))

(defun regular-file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator
PATHSPEC exists and is a regular file. Returns its truename
if this is the case, NIL otherwise. Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :regular-file)))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator
PATHSPEC exists and is a directory.  Returns its truename
if this is the case, NIL otherwise.  Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :directory)))

(defun good-symlink-exists-p (pathspec)
  "Checks whether the file named by the pathname designator
PATHSPEC exists and is a symlink pointing to an existent file."
  (eq :broken (nth-value 1 (file-kind pathspec :follow-symlinks t))))

;;;; Temporary files

(defvar *temporary-directory*
  (let ((system-tmpdir (coerce (environment-variable "TMPDIR") 'string)))
    (if (string= "" system-tmpdir) ; null or empty
        (make-pathname :directory '(:absolute "tmp"))
        (pathname (concatenate 'string system-tmpdir "/")))))

(defun %open-temporary-file/fd-streams (filename element-type external-format)
  (handler-case
      (multiple-value-bind (fd path)
          (#-windows nix:mkstemp #+windows nix:mktemp filename)
        (unwind-protect-case ()
            (nix:unlink path)
          (:abort (nix:close fd)))
        (make-fd-stream fd :direction :io
                        :element-type element-type
                        :external-format external-format
                        :pathname (pathname path)
                        :file path))
    (nix:posix-error ()
      (error 'file-error :pathname filename))))

(defun %open-temporary-file/no-fd-streams (filename element-type external-format)
  (do ((counter 100 (1- counter)))
      ((zerop counter) (error 'file-error :pathname filename))
    (let* ((path (nix:mktemp filename))
           (stream (open path :direction :io
                         :element-type element-type
                         :external-format external-format
                         :if-exists nil
                         :if-does-not-exist :create)))
      (when stream
        (unwind-protect-case ()
          (nix:unlink path)
        (:normal (return stream))
        (:abort (close stream :abort t)))))))

(defun open-temporary-file (&key (pathspec *temporary-directory*)
                            (element-type 'character)
                            (external-format :default))
  "Creates a temporary file setup for input and output, and returns a
stream connected to that file.

PATHSPEC serves as template for the file to be created: a certain number
of random characters will be concatenated to the file component of PATHSPEC.
If PATHSPEC has no directory component, the file will be created inside
*TEMPORARY-DIRECTORY*. The file itself is unlinked once it has been opened.

ELEMENT-TYPE specifies the unit of transaction of the stream.
Consider using WITH-TEMPORARY-FILE instead of this function.

On failure, a FILE-ERROR may be signalled."
  (let ((filename
         (native-namestring
          (merge-pathnames (pathname pathspec) *temporary-directory*))))
    #+osicat-fd-streams
    (%open-temporary-file/fd-streams filename element-type external-format)
    #-osicat-fd-streams
    (%open-temporary-file/no-fd-streams filename element-type external-format)))

(defmacro with-temporary-file ((stream &key (pathspec *temporary-directory*)
                                       (element-type 'character)
                                       (external-format :default))
                               &body body)
  "Within the lexical scope of the body, STREAM is connected to a
temporary file as created by OPEN-TEMPORARY-FILE.  The file is
closed automatically once BODY exits."
  `(with-open-stream
       (,stream (open-temporary-file :pathspec ,pathspec
                                     :element-type ,(if (eq element-type 'character)
                                                        (quote 'character)
                                                        element-type)
                                     :external-format ,external-format))
     ,@body))

;;;; Directory access

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "PATHSPEC must be a valid directory designator:
*DEFAULT-PATHNAME-DEFAULTS* is bound, and (CURRENT-DIRECTORY) is set
to the designated directory for the dynamic scope of the body.

Within the lexical scope of the body, ITERATOR is defined via
macrolet such that successive invocations of (ITERATOR) return
the directory entries, one by one.  Both files and directories
are returned, except '.' and '..'.  The order of entries is not
guaranteed.  The entries are returned as relative pathnames
against the designated directory.  Entries that are symbolic
links are not resolved, but links that point to directories are
interpreted as directory designators.  Once all entries have been
returned, further invocations of (ITERATOR) will all return NIL.

The value returned is the value of the last form evaluated in
body.  Signals an error if PATHSPEC is wild or does not designate
a directory."
  (with-unique-names (one-iter)
    `(call-with-directory-iterator
      ,pathspec
      (lambda (,one-iter)
        (declare (type function ,one-iter))
        (macrolet ((,iterator ()
                     `(funcall ,',one-iter)))
          ,@body)))))

(defun delete-directory (pathspec)
  "Deletes the directory designated by PATHSPEC.  Returns T.  The
directory must be empty.  Symbolic links are not followed.

Signals an error if PATHSPEC is wild, doesn't designate a
directory, or if the directory could not be deleted."
  (zerop (nix:rmdir (absolute-pathname pathspec))))

;;;; File permissions

(define-constant +permissions+
    (append '((:user-read    . #.nix:s-irusr)
              (:user-write   . #.nix:s-iwusr)
              (:user-exec    . #.nix:s-ixusr))
            #-windows ; KLUDGE
            '((:group-read   . #.nix:s-irgrp)
              (:group-write  . #.nix:s-iwgrp)
              (:group-exec   . #.nix:s-ixgrp)
              (:other-read   . #.nix:s-iroth)
              (:other-write  . #.nix:s-iwoth)
              (:other-exec   . #.nix:s-ixoth)
              (:set-user-id  . #.nix:s-isuid)
              (:set-group-id . #.nix:s-isgid)
              (:sticky       . #.nix:s-isvtx)))
  :test #'equal)

(defun file-permissions (pathspec)
  "FILE-PERMISSIONS returns a list of keywords identifying the
permissions of PATHSPEC.

SETF FILE-PERMISSIONS sets the permissions of PATHSPEC as
identified by the symbols in list.

If PATHSPEC designates a symbolic link, that link is implicitly
resolved.

Permission symbols consist of :USER-READ, :USER-WRITE, :USER-EXEC,
:GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ, :OTHER-WRITE,
:OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY.

Both signal an error if PATHSPEC is wild, or doesn't designate an
existing file."
  (let ((mode (nix:stat-mode (nix:stat pathspec))))
    (loop for (name . value) in +permissions+
          when (plusp (logand mode value))
          collect name)))

(defun (setf file-permissions) (perms pathspec)
  (nix:chmod pathspec
             (reduce (lambda (a b)
                       (logior a (cdr (assoc b +permissions+))))
                     perms :initial-value 0)))

;;;; Current directory

(defun current-directory ()
  "CURRENT-DIRECTORY returns the operating system's current
directory, which may or may not correspond to
*DEFAULT-PATHNAME-DEFAULTS*.

SETF CURRENT-DIRECTORY changes the operating system's current
directory to the PATHSPEC.  An error is signalled if the PATHSPEC
is wild or does not designate a directory."
  (let ((cwd (nix:getcwd)))
    (if cwd
        (pathname (concatenate 'string cwd "/"))
        (system-error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  (nix:chdir pathspec))