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

(def-function ("osicat_mode" c-file-mode) ((name :cstring))
  :module "osicat"
  :returning :int)

(define-condition bug (error) 
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream)
	     (format stream "~A. This seems to be a bug in Osicat.~
                             Please report on osicat-devel@common-lisp.net."
		     (message condition)))))

(eval `(defun c-file-kind (c-file)
	 (let ((mode (c-file-mode c-file)))
	   (unless (minusp mode)
	     (case (logand mode-mask mode)
	       ,@(mapcar
		  (lambda (sym)
		    (list (eval sym)
			  (intern (symbol-name sym) :keyword)))
		  '(directory char-device block-device
		    regular-file symbolic-link pipe socket))
	       (t (error
		   'bug :message
		   (format nil "Unknown file mode: ~H." mode))))))))

(defmacro with-c-file ((c-file pathname &optional required-kind) &body forms)
  ;; FIXME: This assumes that OS has the same idea of current dir as Lisp
  (with-unique-names (path)
    `(let ((,path ,pathname))
       (with-cstring (,c-file (namestring ,path))
	 ,(etypecase required-kind
	     (keyword `(let ((real-kind (c-file-kind ,c-file)))
			 (unless (eq ,required-kind real-kind)
			   (if real-kind
			       (error "~A is ~A, not ~A."
				      ,path real-kind ,required-kind)
			       (error "~A ~S does not exist."
				      ,required-kind ,path)))))
	     ((eql t) `(unless (c-file-kind ,c-file)
			 (error "~A does not exist." ,path)))
	     (null nil))
	 ,@forms))))

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

;;; FIXME: Documentation, DIRECTORY-LIST?

(defmacro with-directory-iterator ((iterator pathspec) &body forms)
  (with-unique-names (dp dir cdir err default)
    `(let ((,dir ,pathspec))
       (with-c-file (,cdir ,dir :directory)
	 (let ((,dp nil)
	       (,default (make-pathname :name nil
					:version nil
					:type nil
					:defaults ,dir)))
	   (unwind-protect
		(flet ((,iterator ()
			 (let ((entry (readdir ,dp)))
			   (if (null-pointer-p entry)
			       nil
			       (merge-pathnames
				(convert-from-cstring
				 (osicat-dirent-name entry))
				,default)))))
		  (setf ,dp (opendir ,cdir))
		  (when (null-pointer-p ,dp)
		    (error "Error opening directory ~S." ,dir))
		  ,@forms)
	     (when ,dp
	       (if (zerop (closedir ,dp))
		   nil
		   (error "Error closing directory ~S." ,dir)))))))))

(defun mapdir (function pathspec)
  "Applies FUNCTION to each entry in DIRECTORY in turn and returns a
list of the results."
  (with-directory-iterator (next pathspec)
    (loop for entry = (next)
	  while entry
	  collect (funcall function entry))))
  
(def-function "rmdir" ((name :cstring))
    :module "osicat"
    :returning :int)

(defun delete-directory (pathspec)
  "Deletes DIRECTORY, which must be empty." 
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
  "Returns the environment variable identified by NAME, or NIL if one
does not exist. NAME can either be a symbol or a string."
  (with-c-name (cname name)
    (copy-seq (convert-from-cstring (getenv cname)))))

(defun (setf environment-variable) (value name)
  "Sets the environment variable identified by NAME to VALUE. Both
NAME and VALUE can be either a symbol or a string. Signals an error on
failure."
  (with-c-name (cname name)
    (with-c-name (cvalue value)
      (if (zerop (setenv cname cvalue 1))
	  value
	  (error "Could not set environment variable ~S to ~S." name value)))))

(defun makunbound-environment-variable (name)
  "Removes the environemtn variable identified by NAME from the
current environment. NAME can be either a string or a symbol. Signals
an error on failure."
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
      "The current environment as a read-only assoc-list. To modify
the environment use (SETF ENVIRONMENT-VARIABLE) and
MAKUNBOUND-ENVIRONMENT-VARIABLE.")
  
(def-function "readlink"
    ((name :cstring) (buffer (* :unsigned-char)) (size :size-t))
  :module "osicat"
  :returning :int)

(defun read-link (pathspec)
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

(defun make-link (target link)
  "Creates LINK as a symbolic link to TARGET."
  (with-c-file (old target t)
    (with-c-file (new link)
      (if (zerop (symlink old new))
	  link
	  (error "Could not make symbolic link ~S -> ~S." link target)))))

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
  "Returns a list of keywords identifying the permissions of
PATHSPEC. Permission symbols consist of :USER-READ, :USER-WRITE,
:USER-EXEC, :GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ,
:OTHER-WRITE, :OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY."
  (with-c-file (path pathspec t)
    (let ((mode (osicat-mode path)))
      (loop for (name . value) in +permissions+
	    when (plusp (logand mode value))
	    collecting name))))

(defun (setf file-permissions) (perms pathspec)
  "Sets the permissions of PATHSPEC as identified by the symbols in
list PERMS. Permission symbols consist of :USER-READ, :USER-WRITE,
:USER-EXEC, :GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ,
:OTHER-WRITE, :OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY."
  (with-c-file (path pathspec t)
    (if (zerop (chmod path (reduce (lambda (a b)
				     (logior a (cdr (assoc b +permissions+))))
				   perms
				   :initial-value 0)))
	perms
	(error "Could not set file permissions of ~S to ~S." pathspec perms))))
