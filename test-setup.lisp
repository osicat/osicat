(defpackage :osicat-test
  (:use :cl :rt :osicat))

(in-package :osicat-test)

;;; Utilities

(defun ensure-file (file)
  (or (probe-file file)
      (with-open-file (f file :direction :output)
	(probe-file f))))

(defun ensure-link (old new)
  (ecase (file-kind new)
    (:symbolic-link new)
    (nil (make-link old new))))

;;; Test environment

(defparameter *test-dir* 
  (ensure-directories-exist 
   (merge-pathnames (make-pathname :directory '(:relative "tmp-test-dir"))
		    (make-pathname :directory (pathname-directory *load-truename*)))))

(defparameter *test-file*
  (with-open-file (f (make-pathname :name "tmp-test-file" :defaults *test-dir*)
		     :direction :output
		     :if-exists :supersede)
    (write-line "testing..." f)
    (truename f)))

(defparameter *test-symlink*
  (ensure-link *test-file* (make-pathname :name "tmp-test-symlink" :defaults *test-dir*)))
  
(assert (every 'probe-file (list *test-dir* *test-file* *test-symlink*)))
