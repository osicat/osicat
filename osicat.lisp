;; Copyright (c) 2003, 2004 Nikodemus Siivola
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :osicat)

(defparameter *osicat-version* 
  #.(with-open-file (f (merge-pathnames "version.txt"
					*compile-file-truename*))
      (symbol-name (read f))))

;;;; COMMON SUBROUTINES

(declaim (inline c-file-kind))
(macrolet ((def ()
	       `(defun c-file-kind (c-file follow-p)
		  (let ((mode (c-file-mode c-file (if follow-p 1 0))))
		    (unless (minusp mode)
		      (case (logand mode-mask mode)
			,@(mapcar
			   (lambda (sym)
			     (list (eval sym)
				   (intern (symbol-name sym) :keyword)))
			   ;; OAOOM Warning: 
			   ;; These are in grovel-constants.lisp as well.
			   '(directory character-device block-device
			     regular-file symbolic-link pipe socket))
			(t (error
			    'bug :message
			    (format nil "Unknown file mode: ~H." mode)))))))))
  (def))

(defmacro with-c-file 
    ((c-file pathname &optional required-kind follow-p) &body forms)
  (with-unique-names (path kind)
    `(let ((,path ,pathname))
       (when (wild-pathname-p ,path)
	 (error "Pathname is wild: ~S." ,path))
       (with-cstring (,c-file (namestring ,path))
	 ,@(if required-kind
	       `((let ((,kind (c-file-kind ,c-file ,follow-p)))
		   ,(etypecase required-kind
		       (keyword `(unless (eq ,required-kind ,kind)
				   (if ,kind
				       (error "~A is ~A, not ~A."
					      ,path ,kind ,required-kind)
				       (error "~A ~S does not exist."
					      ,required-kind ,path))))
		       ((eql t) `(unless ,kind
				   (error "~A does not exist." ,path))))
		   ,@forms))
	       forms)))))

(defun relative-pathname-p (pathspec)
  (not (eq :absolute (car (pathname-directory pathspec)))))

(defun absolute-pathname (pathspec &optional (default *default-pathname-defaults*))
  (if (relative-pathname-p pathspec)
      (let ((tmp (merge-pathnames 
		  pathspec
		  (make-pathname :name nil :type nil :version nil
				 :defaults default))))
	(if (relative-pathname-p tmp)
	    (merge-pathnames tmp (current-directory))
	    tmp))
      pathspec))

(defun escape-wild-name (name)
  (declare (simple-string name))
  (let (stack)
    (loop for char across name
	  when (member char '(#\* #\[))
	  do (push #\\ stack)
	  do (push char stack))
    (coerce (nreverse stack) 'simple-string)))

(defun unmerge-pathnames
    (pathspec &optional (known *default-pathname-defaults*))
  (let* ((dir (pathname-directory pathspec))
	 (mismatch (mismatch dir (pathname-directory known) :test #'equal)))
    (make-pathname 
     :directory (when mismatch
		  `(:relative ,@(subseq dir mismatch)))
     :defaults pathspec)))

(defun normpath (pathspec &optional absolute)
  (flet ((fixedname (path)
	   (let ((name (pathname-name path)))
	     (cond ((equal ".." name) :up)
		   ((equal "." name) nil)
		   ((stringp name) name))))
	 (fixedtype (path)
	   (let ((type (pathname-type path)))
	     (and (stringp type) type)))
	 (fixeddir (path)
	   (let ((dir (pathname-directory (concatenate 'string
						       (namestring path)
						       "/"))))
	     (if (member (car dir) '(:absolute :relative))
		 dir
		 (cons :relative dir)))))
    (let ((path (absolute-pathname pathspec)))
      (with-cstring (cfile (namestring path))
	(let ((abspath (if (eq :directory (c-file-kind cfile t))
			   (make-pathname :name nil :type nil
					  :directory (fixeddir path)
					  :defaults path)
			   path)))
	  (if absolute
	      abspath
	      (unmerge-pathnames abspath)))))))

;;;; FILE-KIND

(defun file-kind (pathspec)
  "function FILE-KIND pathspec => file-kind

Returns a keyword indicating the kind of file designated by pathspec,
or NIL if the file does not exist. Does not follow symbolic links.

Possible file-kinds in addition to NIL are: :regular-file,
:symbolic-link, :directory,:pipe, :socket, :character-device, and
:block-device.

Signals an error if pathspec is wild."
  (let ((path (merge-pathnames pathspec)))
    (when (wild-pathname-p path)
      (error "Pathname is wild: ~S." path))
    (with-cstring (cfile (namestring path))
      (c-file-kind cfile nil))))

;;;; DIRECTORY ACCESS

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "macro WITH-DIRECTORY-ITERATOR (iterator pathspec) &body forms => value

The directory designated by pathspec is then bound to 
*default-pathname-defaults* for the dynamic scope of the body.

Within the lexical scope of the body, iterator is defined via macrolet
such that successive invocations of (iterator) return the directory
entries, one by one. Both files and directories are returned, except
'.' and '..'. The order of entries is not guaranteed. The entries are
returned as relative pathnames against the directory. Entries that are
symbolic links are not resolved. Once all entries have been returned, 
further invocations of (iterator) will all return NIL.

The value returned is the value of the last form evaluated in
body. Signals an error if pathspec is wild or does not designate a directory."
  (with-unique-names (dp dir cdir one-iter)
    `(let ((,dir (normpath ,pathspec t)))
       (with-c-file (,cdir ,dir :directory t)
	 (let (,dp)
	   (unwind-protect
		(labels ((,one-iter ()
			   (let ((entry (readdir ,dp)))
			     (if (null-pointer-p entry)
				 nil
				 (let ((string
					(convert-from-cstring
					 (osicat-dirent-name entry))))
				   (if (member string '("." "..") 
					       :test #'string=)
				       (,one-iter)
				       (normpath (escape-wild-name string))))))))
		  (macrolet ((,iterator () 
			       `(,',one-iter)))
		    (setf ,dp (opendir ,cdir))
		    (when (null-pointer-p ,dp)
		      (error "Error opening directory ~S." ,dir))
		    (let ((*default-pathname-defaults* ,dir))
		      ,@body)))
	     (when ,dp
	       (if (zerop (closedir ,dp))
		   nil
		   (error "Error closing directory ~S." ,dir)))))))))

(defun mapdir (function pathspec)
  "function MAPDIR function pathspec => list

Applies function to each entry in directory designated by pathspec in
turn and returns a list of the results. Binds 
*default-pathname-defaults* to the directory designated by pathspec
round to function call.

If pathspec designates a symbolic link, it is implicitly resolved.

Signals an error if pathspec is wild or doesn't designate a directory."
  (with-directory-iterator (next pathspec)
    (loop for entry = (next)
	  while entry
	  collect (funcall function entry))))

(defun delete-directory (pathspec)
  "function DELETE-DIRECTORY pathspec => T

Deletes the directory designated by pathspec. Returns T.  The
directory must be empty. Symbolic links are not followed.

Signals an error if pathspec is wild, doesn't designate a directory,
or if the directory could not be deleted."
  (with-c-file (path (normpath pathspec t) :directory)
    (if (zerop (rmdir path))
	pathspec
	(error "Could not delete directory ~S." pathspec))))

(defun environment-variable (name)
  "function ENVIRONMENT-VARIABLE name => string
function (SETF (ENVIRONMENT-VARIABLE name) value) => value

ENVIRONMENT-VARIABLE returns the environment variable identified by
name, or NIL if one does not exist. Name can either be a symbol or a
string.

SETF ENVIRONMENT-VARIABLE sets the environment variable identified by
name to value. Both name and value can be either a symbols or
strings. Signals an error on failure."
  (with-c-name (cname name)
    (copy-seq (convert-from-cstring (getenv cname)))))

(defun (setf environment-variable) (value name)
  (with-c-name (cname name)
    (with-c-name (cvalue value)
      (if (zerop (setenv cname cvalue 1))
	  (convert-from-cstring cvalue)
	  (error "Could not set environment variable ~S to ~S." name value)))))

(defun makunbound-environment-variable (name)
  "function MAKUNBOUND-ENVIRONMENT-VARIABLE name => string

Removes the environment variable identified by name from the current
environment. name can be either a string or a symbol. Returns the
string designated by name. Signals an error on failure."
  (with-c-name (cname name)
    (if (zerop (unsetenv cname))
	(string name)
	(error "Could not remove environment variable ~S." name))))

(defun environment ()
  "function ENVIRONMENT => alist
function (SETF ENVIRONMENT) alist => alist

ENVIRONMENT return the current environment as an assoc-list. 
SETF ENVIRONMENT modifies the environment its argument. 

Often it is preferable to use SETF ENVIRONMENT-VARIABLE and
MAKUNBOUND-ENVIRONMENT-VARIABLE to modify the environment instead
of SETF ENVIRONMENT."
  (handler-case
      (loop for i from 0 by 1
	    for string = (convert-from-cstring
			  (deref-array environ cstring-array i))
	    for split = (position #\= string)
	    while string
	    collecting (cons (subseq string 0 split) 
			     (subseq string (1+ split))))
    (error (e)
      (error "Could not access environment (~S)." e))))

(defun (setf environment) (alist)
  (let ((oldenv (get-environ)))
    (loop for (var . val) in alist
	  do (setf (environment-variable var) (string val)
		   oldenv (delete var oldenv 
				  :key (lambda (x) (string (car x)))
				  :test #'string=)))
    (loop for (var . val) in oldenv
	  do (makunbound-environment-variable var)))
  alist)

(defun read-link (pathspec)
  "function READ-LINK pathspec => pathname

Returns the pathname pointed to by the symbolic link designated by
pathspec. If the link is relative, then the returned pathname is
relative to the link, not *default-pathname-defaults*.

Signals an error if pathspec is wild, or does not designate a symbolic
link."
  (handler-bind
      (#+sbcl (sb-ext:compiler-note #'muffle-warning))
    (with-c-file (path (normpath pathspec t) :symbolic-link)
      (do* ((size 64 (* size 2))
	    (buffer #1=(allocate-foreign-string size) #1#)
	    (got (readlink path buffer size)))
	   ((< got size)
	    (let ((str (convert-from-foreign-string buffer :length got)))
	      (free-foreign-object buffer)
	      (pathname str)))
	(free-foreign-object buffer)))))

(defun make-link (link &key target hard)
  "function MAKE-LINK link &key target hard => pathname

Creates link that points to target. Defaults to a symbolic link, but
giving a non-NIL value to the keyword argument :HARD creates a hard
link. Returns the pathname of the link. 

Relative targets are resolved against the link. Relative links are 
resolved against *default-pathname-defaults*.

Signals an error if either target or link is wild, target does not
exist, or link exists already."
  (unless target
    (error "No target given to MAKE-LINK."))
  (let ((old (current-directory)))
    (unwind-protect
	 ;; KLUDGE: We merge against link for hard links only,
	 ;; since symlink does the right thing once we are in
	 ;; the correct directory.
	 (with-c-file (old (if hard (merge-pathnames target link) target))
	   (with-c-file (new link)
	     (setf (current-directory) 
		   (normpath *default-pathname-defaults* t))
	     (if (zerop (funcall (if hard #'link #'symlink) old new))
		 (pathname link)
		 (error "MAKE-LINK: Could not create ~A link ~S -> ~S." 
			(if hard "hard" "symbolic") new old))))
      (setf (current-directory) old))))

(define-symbol-macro +permissions+
    (load-time-value (mapcar (lambda (x)
			       (cons (intern (symbol-name x) :keyword)
				     (eval x)))
			     '(user-read user-write user-exec
			       group-read group-write group-exec
			       other-read other-write other-exec
			       set-user-id set-group-id sticky))))

(defun file-permissions (pathspec)
  "function FILE-PERMISSIONS pathspec => list
function (SETF (FILE-PERMISSIONS pathspec) list) => list

FILE-PERMISSIONS returns a list of keywords identifying the
permissions of pathspec.

SETF FILE-PERMISSIONS sets the permissions of pathspec as identified
by the symbols in list.

If pathspec designates a symbolic link, that link is implicitly
resolved.

Permission symbols consist of :user-read, :user-write, :user-exec,
:group-read, :group-write, :group-exec, :other-read, :other-write,
:other-exec, :set-user-id, :set-group-id, and :sticky.

Both signal an error is pathspec is wild, or doesn't designate an
exiting file."
  (with-c-file (path pathspec t t)
    ;; FIXME: We stat twice here.
    (let ((mode (c-file-mode path 1)))
      (loop for (name . value) in +permissions+
	    when (plusp (logand mode value))
	    collecting name))))

(defun (setf file-permissions) (perms pathspec)
  (with-c-file (path pathspec t t)
    (if (zerop (chmod path (reduce (lambda (a b)
				     (logior a (cdr (assoc b +permissions+))))
				   perms
				   :initial-value 0)))
	perms
	(error "Could not set file permissions of ~S to ~S." pathspec perms))))

(defun current-directory ()
  "function CURRENT-DIRECTORY => pathname
function (SETF CURRENT-DIRECTORY) pathspec => pathspec

CURRENT-DIRECTORY returns the operating system's current directory, which
may or may not correspond to *DEFAULT-PATHNAME-DEFAULTS*.

SETF CURRENT-DIRECTORY changes the operating system's current directory to
the pathspec. An error is signalled if the pathspec is wild or does not 
designate a directory."
  (let* ((cwd (c-getcwd))
	 (str (convert-from-foreign-string cwd :null-terminated-p t)))
    (if str
	(prog1
	    (pathname (concatenate 'string str "/"))
	  (free-foreign-object cwd))
	(error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  (with-c-file (dir pathspec :directory)
    (if (minusp (chdir dir))
	(error "Could not change current directory.")
	pathspec)))
