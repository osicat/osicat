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

(defpackage :osicat
  (:use :cl :uffi)
  (:export
   ;;; Evironment
   #:environment
   #:environment-variable
   #:makunbound-environment-variable
   ;; Directories
   #:with-directory-iterator
   #:delete-directory
   ;; Files
   #:file-kind
   ;; #:file-permissions
   ;; #:read-link
   ;; #:make-link
   ;; Processes
   ;; #:run-shell-command
   ;; #:fork
   ))

(in-package :osicat)

;; some implementations don't like broken symlinks
(def-function "osicat_probe_file" ((name :cstring))
  :module "osicat"
  :returning :int)

(defmacro c-namestring (pathspec)
  `(convert-to-cstring (namestring ,pathspec)))

(defun probe-file-carefully (pathspec)
  (zerop (osicat-probe-file (c-namestring pathspec))))

(def-function "osicat_file_type" ((name :cstring))
  :module "osicat"
  :returning :int)

(defun symcat (&rest symbols)
  (intern (apply 'concatenate 'string (mapcar 'symbol-name symbols))))

(let ((types nil))
  (macrolet ((def (clear code)
		 (let ((cvar (concatenate 'string "osicat_if" code))
		       (lvar (symcat 'file-if (intern (string-upcase code))
				     '-tag))
		       (pred (symcat 'file- clear '-p)))
		   (eval `(def-foreign-var (,cvar ,lvar)
			      :unsigned-int "osicat"))
		   `(push (list ,lvar
				(intern (symbol-name ',clear) :keyword))
			  types))))
      (def directory "dir")
      (def char-device "chr")
      (def block-device "blk")
      (def regular "reg")
      (def link "lnk")
      (def pipe "ifo")
      (def socket "sock"))

  (eval `(defun file-kind (pathspec)
	   "Returns a keyword identifying the type of file (one of
:REGULAR, :DIRECTORY, :LINK, :CHAR-DEVICE, :BLOCK-DEVICE, :PIPE, or
:SOCKET). Symbolic links are not followed. Error is signalled on
failure."
	   (if (probe-file-carefully pathspec)
	       (case (osicat-file-type (c-namestring pathspec))
		 ,@types
		 (t :unknown))
	       (error "File does not exits: ~S." pathspec)))))

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

(defmacro with-directory-iterator ((iterator name) &body forms)
  (with-unique-names (dp dir def)
    `(let ((,dir ,name))
       (if (eq :directory (file-kind ,dir))
	   (let ((,dp (opendir (c-namestring ,dir)))
		 (,def (make-pathname :name nil
				      :version nil
				      :type nil
				      :defaults ,dir)))
	     (if (null-pointer-p ,dp)
		 (error "Error opening directory ~S." ,dir)
		 (unwind-protect
		      (macrolet ((,iterator ()
				   `(let ((entry (readdir ,',dp)))
				      (if (null-pointer-p entry)
					  nil
					  (merge-pathnames
					   (convert-from-cstring
					    (osicat-dirent-name entry))
					   ,def)))))
			,@forms)
		   (if (zerop (closedir ,dp))
		       nil
		       (error "Error closing directory ~S." ,dir)))))
	   (error "Not a directory: ~S." ,dir)))))

(def-function "rmdir" ((name :cstring))
    :module "osicat"
    :returning :int)

(defun delete-directory (pathspec)
  (if (zerop (rmdir (c-namestring pathspec)))
      pathspec
      (error "Could not delete directory ~S." pathspec)))


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

(defun ensure-c-name (obj)
  (convert-to-cstring
   (if (stringp obj)
       obj
       (with-standard-io-syntax      
	 (princ-to-string obj)))))

(defun environment-variable (name)
  "Returns the environment variable identified by NAME, or NIL if one
does not exist. If NAME is not a string it will be converted to one as
if by PRINC-TO-STRING using standard IO syntax. In practice this means
that common POSIX-style all-caps environment variables can be referred
to by symbols."
  (copy-seq (convert-from-cstring (getenv (ensure-c-name name)))))

(defun (setf environment-variable) (value name)
  "Sets the environment variable identified by NAME to VALUE. If NAME
is not a string it will be converted to one as if by PRINC-TO-STRING
using standard IO syntax, but VALUE is passed as is, and is required
to be a string. In practice this means that environment variables can
be referred to by symbols. Signals an error on failure."
  (if (zerop (setenv (ensure-c-name name)
		     (convert-to-cstring (copy-seq value))
		     1))
      value
      (error "Could not set environment variable ~S to ~S." name value)))

(defun makunbound-environment-variable (name)
  "Removes the environemtn variable identified by NAME from the
current environment. If NAME is not a string it will be converted to
one as if by PRINC-TO-STRING using standard IO syntax. In practice
this means that environment variables can be referred to by
symbols. Signals an error on failure."
  (if (zerop (unsetenv (ensure-c-name name)))
      nil
      (error "Could not remove environment variable ~S." name)))

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
