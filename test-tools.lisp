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

(defpackage :osicat-test
  (:use :cl :rt :osicat)
  (:export
   #:setup
   #:teardown))

(in-package :osicat-test)

;;; Utilities

(defvar *test-dir* 
  (merge-pathnames 
   (make-pathname :directory '(:relative "tmp-test-dir"))
   (make-pathname :directory 
		  (pathname-directory #.*compile-file-truename*))))

(defun ensure-file (file &optional (dir *test-dir*))
  (let ((file (merge-pathnames file dir)))
    (or (probe-file file)
	(with-open-file (f file :direction :output)
	  (probe-file f)))))

(defun ensure-link (link &key target)
  (let* ((link (merge-pathnames link *test-dir*))
	 (target (merge-pathnames target *test-dir*))
	 (kind (file-kind link)))
    (cond ((eq :symbolic-link kind) link)
	  ((null kind) (make-link link :target target))
	  (t (error "File exists and is not a link.")))))

;;; Test environment

(defun teardown ()
  (assert (search "tmp-test-dir" (namestring *test-dir*)))
  (asdf:run-shell-command "rm -rf ~A" (namestring *test-dir*)))

(defun setup ()
  (teardown)
  (ensure-directories-exist *test-dir*))


