;; Copyright (c) 2003, 2004 Nikodemus Siivola, Julian Squires
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

(defparameter *osicat-version* '(0 4 1)
  "variable *OSICAT-VERSION*

Osicat version number represented as a list of three integers. The
three integers represent major, minor, and revision versions.")

;;;; Common subroutines

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
			    (format nil "Unknown file mode: ~A." mode)))))))))
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

;;;; Hopefully portable pathname manipulations

(defun absolute-pathname-p (pathspec)
  "function ABSOLUTE-PATHNAME-P pathspec => boolean

Returns T if the pathspec designates an absolute pathname, NIL otherwise."
  (eq :absolute (car (pathname-directory pathspec))))

(defun relative-pathname-p (pathspec)
  "function RELATIVE-PATHNAME-p pathspec => boolean

Returns T if the pathspec designates a relative pathname, NIL otherwise."
  (not (absolute-pathname-p pathspec)))

(defun absolute-pathname (pathspec &optional (default *default-pathname-defaults*))
  "function ABSOLUTE-PATHNAME pathspec &optional default => pathname

Returns an absolute pathname corresponding to pathspec by merging it with default,
and (current-directory) if necessary."
  (if (relative-pathname-p pathspec)
      (let ((tmp (merge-pathnames 
		  pathspec
		  (make-pathname :name nil :type nil :version nil
				 :defaults default))))
	(if (relative-pathname-p tmp)
	    (merge-pathnames tmp (current-directory))
	    tmp))
      pathspec))

(defun unmerge-pathnames (pathspec &optional (default *default-pathname-defaults*))
  "function UNMERGE-PATHNAMES pathspec &optional default => pathname

Removes those leading directory components from pathspec that are
shared with default."
  (let* ((dir (pathname-directory pathspec))
	 (mismatch (mismatch dir (pathname-directory default) :test #'equal)))
    (make-pathname 
     :directory (when mismatch
		  `(:relative ,@(subseq dir mismatch)))
     :defaults pathspec)))

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

;;;; Temporary files

(defun make-temporary-file (&key (element-type 'character))
  "function MAKE-TEMPORARY-FILE (&key element-type) => stream

Makes a temporary file setup for input and output, and returns a
stream connected to that file.  ELEMENT-TYPE specifies the unit of
transaction of the stream.

On failure, a FILE-ERROR may be signalled."
  #+(or cmu sbcl)
  (let ((fd (osicat-tmpfile)))
    (unless (>= fd 0) (signal 'file-error))
    #+cmu(sys:make-fd-stream fd :input t :output t
			     :element-type element-type)
    #+sbcl(sb-sys:make-fd-stream fd :input t :output t
				 :element-type element-type))
  ;; XXX Warn about insecurity?  Or is any platform too dumb to have
  ;; fds, also relatively safe from race conditions through obscurity?
  ;; XXX Another bug with this: the file doesn't get unlinked.
  #-(or cmu sbcl)
  (open (tmpnam nil) :direction :io :element-type element-type))


(defmacro with-temporary-file ((stream &key element-type) &body body)
  "macro WITH-TEMPORARY-FILE (stream &key element-type) &body body => stream"
  `(let ((,stream (make-temporary-file
		   ,@(when element-type
			   `(:element-type ,element-type)))))
    (unwind-protect
	 (progn ,@body)
      (close ,stream :abort t))))

;;;; Directory access

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "macro WITH-DIRECTORY-ITERATOR (iterator pathspec) &body forms => value

Pathspec must be a valid directory designator:
*default-pathname-defaults* is bound, and (current-directory) is set
to the designated directory for the dynamic scope of the body.

Within the lexical scope of the body, iterator is defined via macrolet
such that successive invocations of (iterator) return the directory
entries, one by one. Both files and directories are returned, except
'.' and '..'. The order of entries is not guaranteed. The entries are
returned as relative pathnames against the designated directory.
Entries that are symbolic links are not resolved, but links that point
to directories are interpreted as directory designators. Once all
entries have been returned, further invocations of (iterator) will all
return NIL.

The value returned is the value of the last form evaluated in
body. Signals an error if pathspec is wild or does not designate a
directory."
  (with-unique-names (one-iter)
    `(call-with-directory-iterator ,pathspec
      (lambda (,one-iter)
	(declare (type function ,one-iter))
	(macrolet ((,iterator () 
		     `(funcall ,',one-iter)))
	  ,@body)))))

(defun call-with-directory-iterator (pathspec fun)
  (let ((dir (absolute-pathname pathspec))
	(old-dir (current-directory)))
    (with-c-file (cdir dir :directory t)
      (let (dp)
	(unwind-protect
	     (labels ((one-iter ()
			(let ((entry (readdir dp)))
			  (if (null-pointer-p entry)
			      nil
			      (let* ((cname (osicat-dirent-name entry))
				     (name (convert-from-cstring cname)))
				(declare (type simple-string name))
				(cond 
				  ((member name '("." "..") :test #'string=)
				      (one-iter))
				  ((eq :directory (c-file-kind cname t))
				   (make-pathname
				    :directory `(:relative ,name)))
				  (t
				   (let ((dotpos (position #\. name :from-end t)))
				     (if (and dotpos (plusp dotpos))
					 (make-pathname
					  :name (subseq name 0 dotpos)
					  :type (subseq name (1+ dotpos)))
					 (make-pathname
					  :name name))))))))))
	       (setf dp (opendir cdir))
	       (when (null-pointer-p dp)
		 (error "Error opening directory ~S." dir))
	       (let ((*default-pathname-defaults* dir))
		 (setf (current-directory) dir)
		 (funcall fun #'one-iter)))
	  (when dp
	    (if (zerop (closedir dp))
		nil
		(error "Error closing directory ~S." dir)))
	  (setf (current-directory) old-dir))))))

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
  (with-c-file (path (absolute-pathname pathspec) :directory)
    (if (zerop (rmdir path))
	pathspec
	(error "Could not delete directory ~S." pathspec))))

;;;; Environment access

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
  (let ((oldenv (environment)))
    (loop for (var . val) in alist
	  do (setf (environment-variable var) (string val)
		   oldenv (delete var oldenv 
				  :key (lambda (x) (string (car x)))
				  :test #'string=)))
    (loop for (var . val) in oldenv
	  do (makunbound-environment-variable var)))
  alist)

;;;; Symbolic and hard links

(defun read-link (pathspec)
  "function READ-LINK pathspec => pathname

Returns the pathname pointed to by the symbolic link designated by
pathspec. If the link is relative, then the returned pathname is
relative to the link, not *default-pathname-defaults*.

Signals an error if pathspec is wild, or does not designate a symbolic
link."
  (handler-bind
      ;; FIXME: Declare types properly to get rid of this.
      (#+sbcl (sb-ext:compiler-note #'muffle-warning))
    (with-c-file (path (absolute-pathname pathspec) :symbolic-link)
      (do* ((size 64 (* size 2))
	    (buffer #1=(allocate-foreign-string size) #1#)
	    (got #2=(readlink path buffer size) #2#))
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
		   (absolute-pathname *default-pathname-defaults*))
	     (if (zerop (funcall (if hard #'link #'symlink) old new))
		 (pathname link)
		 (error "MAKE-LINK: Could not create ~A link ~S -> ~S." 
			(if hard "hard" "symbolic") new old))))
      (setf (current-directory) old))))

;;;; File permissions

(defconstant +permissions+ (if (boundp '+permissions+)
			       (symbol-value '+permissions+)
			       (mapcar (lambda (x)
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

;;;; Current directory

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

;;;; USER INFORMATION

(defun user-info (id)
  "function USER-INFO name => alist
function USER-INFO user-id => alist

USER-INFO returns the password entry for the given name or numerical
user ID, as an assoc-list."
  (let ((pwent (typecase id
		 (string (with-cstring (name id) (getpwnam name)))
		 (integer (getpwuid id))
		 (t (make-null-pointer :pointer-void)))))
    (when (not (null-pointer-p pwent))
      (list (cons :name (osicat-pwent-name pwent))
	    (cons :user-id (osicat-pwent-uid pwent))
	    (cons :group-id (osicat-pwent-gid pwent))
	    (cons :gecos (osicat-pwent-gecos pwent))
	    (cons :home (osicat-pwent-home pwent))
	    (cons :shell (osicat-pwent-shell pwent))))))
