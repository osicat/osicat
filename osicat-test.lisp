(defpackage :osicat-test
  (:use :cl :rt :osicat))

(in-package :osicat-test)

(defvar *test-dir* 
  (ignore-errors
    (ensure-directories-exist 
     (merge-pathnames (make-pathname :directory '(:relative "tmp-test-dir"))
		      (make-pathname :directory (pathname-directory *load-truename*))))))

(defvar *test-file*
  (ignore-errors
    (with-open-file (f (make-pathname :name "tmp-test-file" :defaults *test-dir*)
		       :direction :output
		       :if-exists :supersede)
      (write-line "testing..." f)
      (truename f))))

(defvar *test-symlink*
  (ignore-errors
    (make-link *test-file* (make-pathname :name "tmp-test-symlink" :defaults *test-dir*))))

(defvar *test-hardlink*
  (ignore-errors
    (make-link *test-file* (make-pathname :name "tmp-test-hardlink" :defaults *test-dir*)
	       :hard t)))

;; Sane test-environment
(assert
 (every 'probe-file (list *test-dir* *test-file* *test-symlink* *test-hardlink*)))

(deftest file-kind.1
    (file-kind *test-dir*)
  :directory)

(deftest file-kind.2
    (file-kind (make-pathname :name "does-not-exist" :defaults *test-dir*))
  nil)

(deftest file-kind.3
    (file-kind *test-symlink*)
  :symbolic-link)

(deftest file-kind.4
    (file-kind *test-file*)
  :regular-file)

(deftest file-kind.5
    (file-kind *test-hardlink*)
  :regular-file)
