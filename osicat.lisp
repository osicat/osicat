;; Copyright (c) 2003 Nikodemus Siivola
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

(def-function ("osicat_mode" c-file-mode) ((name :cstring) (follow-p :int))
  :module "osicat"
  :returning :int)

(define-condition bug (error) 
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream)
	     (format stream "~A. This seems to be a bug in Osicat.~
                             Please report on osicat-devel@common-lisp.net."
		     (message condition)))))

;;; KLUDGE: Would macrolet frob be preferable here? I can't see why...
(eval 
 `(defun c-file-kind (c-file follow-p)
    (let ((mode (c-file-mode c-file (if follow-p 1 0))))
      (unless (minusp mode)
	(case (logand mode-mask mode)
	  ,@(mapcar
	     (lambda (sym)
	       (list (eval sym)
		     (intern (symbol-name sym) :keyword)))
	     ;; OAOOM: These are in grovel-constants.lisp as well.
	     '(directory character-device block-device
	       regular-file symbolic-link pipe socket))
	  (t (error
	      'bug :message
	      (format nil "Unknown file mode: ~H." mode))))))))

(defmacro with-c-file ((c-file pathname &optional required-kind follow-p) &body forms)
  ;; FIXME: This assumes that OS has the same idea of current dir as Lisp
  (with-unique-names (path kind)
    `(let ((,path ,pathname))
       (when (wild-pathname-p ,path)
	 (error "Pathname is wild: ~S." ,path))
       (with-cstring (,c-file (namestring ,path))
	 (let ((,kind (c-file-kind ,c-file ,follow-p)))
	   ,(etypecase required-kind
	       (keyword `(unless (eq ,required-kind ,kind)
			   (if ,kind
			       (error "~A is ~A, not ~A."
				      ,path ,kind ,required-kind)
			       (error "~A ~S does not exist."
				      ,required-kind ,path))))
	       ((eql t) `(unless ,kind
			   (error "~A does not exist." ,path)))
	       (null nil))
	   ,@forms)))))

(defun file-kind (pathspec)
  "function FILE-KIND pathspec => file-kind

Returns a keyword indicating the kind of file designated by pathspec,
or NIL if the file does not exist. Does not follow symbolic links.

Possible file-kinds in addition to NIL are: :regular-file,
:symbolic-link, :directory,:pipe, :socket, :character-device, and
:block-device.

Signals an error if pathspec is wild."
  ;; KLUDGE: OAOOM: We scurry to avoid an extra lstat here. 
  (let ((path (pathname pathspec)))
    (when (wild-pathname-p path)
      (error "Pathname is wild: ~S." path))
    (print (list 'namestring (namestring path)))
    (with-cstring (cfile (namestring path))
      (print (list 'cfile cfile))
      (c-file-kind cfile nil))))

(def-function "opendir" ((name :cstring))
  :module "osicat"
  :returning :pointer-void)

(def-function "closedir" ((dir :pointer-void))
  :module "osicat"
  :returning :int)

(def-function "readdir" ((dir :pointer-void))
  :module "osicat"
  :returning :pointer-void)

(def-function "osicat_dirent_name" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "macro WITH-DIRECTORY-ITERATOR (iterator pathspec) &body forms => value

Within the lexical scope of the body, iterator is defined via flet
such that successive invocations of (iterator) return the directory
entries, one by one. Both files and directories are returned, except
'.' and '..'. The order of entries is not guaranteed. 

Once all entries have been returned, further invocations of (iterator)
will all return NIL.

The value returned is the value of the last form evaluated in
body.

If pathspec designates a symbolic link, it is implicitly resolved.

Signal an error if pathspec is wild or does not designate a directory."  
  (with-unique-names (dp dir cdir err default)
    `(let ((,dir ,pathspec))
       (with-c-file (,cdir ,dir :directory t)
	 (let ((,dp nil)
	       (,default (make-pathname :name nil
					:type nil
					:directory (append ;KLUDGE: deal with missing /'s
						    (pathname-directory ,dir)
						    (remove-if (lambda (o)
								 (or (null o)
								     (keywordp o)))
							       (list (pathname-name ,dir)
								     (pathname-type ,dir))))
					:defaults ,dir)))
	   (unwind-protect
		(labels ((,iterator ()
			   (let ((entry (readdir ,dp)))
			     (if (null-pointer-p entry)
				 nil
				 (let ((namestring (convert-from-cstring
						    (osicat-dirent-name entry))))
				   (if (member namestring '("." "..") :test #'equal)
				       (,iterator)
				       (merge-pathnames namestring ,default)))))))
		  (setf ,dp (opendir ,cdir))
		  (when (null-pointer-p ,dp)
		    (error "Error opening directory ~S." ,dir))
		  ,@body)
	     (when ,dp
	       (if (zerop (closedir ,dp))
		   nil
		   (error "Error closing directory ~S." ,dir)))))))))

(defun mapdir (function pathspec)
  "function MAPDIR function pathspec => list

Applies function to each entry in directory designated by pathspec in
turn and returns a list of the results.

If pathspec designates a symbolic link, it is implicitly resolved.

Signals an error if pathspec is wild or doesn't designate a directory."
  (with-directory-iterator (next pathspec)
    (loop for entry = (next)
	  while entry
	  collect (funcall function entry))))
  
(def-function "rmdir" ((name :cstring))
    :module "osicat"
    :returning :int)

(defun delete-directory (pathspec)
  "function DELETE-DIRECTORY pathspec => T

Deletes the direcotry designated by pathspec. Returns T.  The
directory must be empty. Symbolic links are not followed.

Signals an error if pathspec is wild, doesn't designate a directory,
or if the direcotry could not be deleted."
  (with-c-file (path pathspec :directory)
    (if (zerop (rmdir path))
	pathspec
	(error "Could not delete directory ~S." pathspec))))

(def-function "getenv" ((name :cstring))
  :module "osicat"
  :returning :cstring)

(def-function "setenv" ((name :cstring) (value :cstring) (replace :int))
  :module "osicat"
  :returning :int)

(def-function "unsetenv" ((name :cstring))
  :module "osicat"
  :returning :int)

(def-array-pointer cstring-array :cstring)
(def-foreign-var "environ" 'cstring-array "osicat")

(defmacro with-c-name ((cname name) &body forms)
  (with-unique-names (n-name)
    `(let ((,n-name ,name))
       (with-cstring (,cname (etypecase ,n-name
			       (string ,n-name)
			       (symbol (symbol-name ,n-name))))
	 ,@forms))))

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
	nil
	(error "Could not remove environment variable ~S." name))))

(defun get-environ ()
  (loop for i from 0 by 1
	for string = (convert-from-cstring
		      (deref-array environ cstring-array i))
	for split = (position #\= string)
	while string
	collecting (cons (subseq string 0 split) (subseq string (1+ split)))))

(define-symbol-macro environment (get-environ))

(setf (documentation 'environment 'variable)
      "symbol-macro ENVIRONMENT

The current environment as a read-only assoc-list. To modify
the environment use (SETF ENVIRONMENT-VARIABLE) and
MAKUNBOUND-ENVIRONMENT-VARIABLE.")
  
(def-function "readlink"
    ((name :cstring) (buffer (* :unsigned-char)) (size :size-t))
  :module "osicat"
  :returning :int)

(defun read-link (pathspec)
  "function READ-LINK pathspec => pathname

Returns the pathname pointed to by the symbolic link designated by
pathspec. If the link is relative, then the returned pathname is
relative to the link, not *default-pathname-defaults*.

Signals an error if pathspec is wild, or does not designate a symbolic
link."
  (with-c-file (path pathspec :symbolic-link)
    (do* ((size 64 (* size 2))
	  (buffer #1=(allocate-foreign-string size) #1#)
	  (got (readlink path buffer size)))
	 ((< got size)
	  (let ((str (convert-from-foreign-string buffer :length got)))
	    (free-foreign-object buffer)
	    (pathname str)))
      (free-foreign-object buffer))))

(def-function "symlink" ((old :cstring) (new :cstring))
  :module "osicat"
  :returning :int)

(def-function "link" ((old :cstring) (new :cstring))
  :module "osicat"
  :returning :int)

(defun make-link (target link &key hard)
  "function MAKE-LINK target link &key hard => pathname

Creates link that points to target. Defaults to a symbolic link, but
giving a non-NIL value to the keyword argument :HARD creates a hard
link. Returns the pathname of the link. 

Signals an error if either target or link is wild, target does not
exist, or link exists already."
  (with-c-file (old target)
    (with-c-file (new link)
      (if (zerop (funcall (if hard #'link #'symlink) old new))
	  (pathname link)
	  (error "Could not create ~A link ~S -> ~S." 
		 (if hard "hard" "symbolic") link target)))))

(def-function "chmod" ((name :cstring) (mode :mode-t))
  :module "osicat"
  :returning :int)

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

Permission symbols consist of :USER-READ, :USER-WRITE, :USER-EXEC,
:GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ, :OTHER-WRITE,
:OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY.

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
