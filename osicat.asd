;; Copyright (c) 2003, 2004 Nikodemus Siivola <nikodemus@random-state.net>
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

(defpackage :osicat-system
  (:use :cl :asdf))

(in-package :osicat-system)

;;;; C-SOURCE FILE HANDLING

(defvar *gcc* "/usr/bin/gcc")
(defvar *gcc-options* '(#-darwin "-shared"
			#+darwin "-dynamiclib"
			"-fPIC"))

(defmethod output-files ((o compile-op) (c c-source-file))
  (list (make-pathname :name (component-name c)
		       :type "so"
		       :defaults (component-pathname c))))

(defmethod perform ((o load-op) (c c-source-file))
  (let ((loader (intern "LOAD-FOREIGN-LIBRARY" :uffi)))
    (dolist (file (asdf::input-files o c))
      (funcall loader file :module "osicat" :force-load t))))

(defmethod perform ((o compile-op) (c c-source-file))
  (unless (zerop (run-shell-command "~A ~A ~{~A ~}-o ~A"
				    *gcc*
				    (namestring (component-pathname c))
				    *gcc-options*
				    (namestring (car (output-files o c)))))
    (error 'operation-error :component c :operation o)))

;;;; GROVELING

(defclass grovel-file (cl-source-file) ())

(defmethod perform ((o compile-op) (c grovel-file))
  (let* ((output-file (car (output-files o c)))
	 (filename (component-pathname c))
	 (c-source (merge-pathnames "tmp.c" output-file))
	 (a-dot-out (merge-pathnames "a.out" output-file))
	 (constants (merge-pathnames "grovel.lisp-temp" output-file))
	 (*grovel*))
    (declare (special *grovel*))
    ;; Loading the groveler will bind the *govel* hook.
    (load filename)    
    (and (funcall (the function *grovel*) c-source a-dot-out constants)
	 (compile-file constants :output-file output-file))))

;;;; SYSTEM

(defsystem :osicat
    :version "0.4.1"
    :depends-on (:uffi)
    :components
    ((:c-source-file "osicat-glue")
     (:file "packages")
     (:grovel-file "grovel-constants" :depends-on ("packages"))
     (:file "early-util" :depends-on ("packages"))
     (:file "ffi" :depends-on ("early-util"))
     (:file "osicat" :depends-on
	    ("osicat-glue" "ffi" "grovel-constants"))))

;;;; TESTING

(defsystem :osicat-test
    :depends-on (:osicat :rt #+sbcl :sb-posix)
    :components ((:file "test-tools")
		 (:file "test-osicat" :depends-on ("test-tools"))))

(defmethod perform ((o test-op) (c (eql (find-system :osicat))))
  (operate 'load-op :osicat-test)
  (funcall (intern "SETUP" :osicat-test))
  (unwind-protect
       (operate 'test-op :osicat-test :force t)
    (funcall (intern "TEARDOWN" :osicat-test))))

(defmethod perform ((o test-op) (c (eql (find-system :osicat-test))))
  (or (funcall (intern "DO-TESTS" :rt))
      (error "test-op failed")))
