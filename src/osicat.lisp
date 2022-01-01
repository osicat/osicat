;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; osicat.lisp --- High-level OS interface for non Windows systems
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

;;;; Directory access
(defun call-with-directory-iterator (pathspec fun)
  (let ((dir (absolute-pathname (pathname pathspec)))
        (old-dir (current-directory)))
    (let ((dp (nix:opendir dir)))
      (labels ((one-iter ()
                 (let ((name (nix:readdir dp)))
                   (unless (null name)
                     (cond
                       ((member name '("." "..") :test #'string=)
                        (one-iter))
                       ((eq :directory (%get-file-kind name t))
                        (make-pathname :directory `(:relative ,name)))
                       (t
                        (let ((dotpos (position #\. name :from-end t)))
                          (if (and dotpos (plusp dotpos))
                              (make-pathname :name (subseq name 0 dotpos)
                                             :type (subseq name (1+ dotpos)))
                              (make-pathname :name name)))))))))
        (unwind-protect
             (let ((*default-pathname-defaults* dir))
               (setf (current-directory) dir)
               (funcall fun #'one-iter))
          (nix:closedir dp)
          (setf (current-directory) old-dir))))))

;;;; USER INFORMATION

(defun user-info (id)
  "USER-INFO returns the password entry for the given name or
numerical user ID, as an assoc-list."
  (multiple-value-bind (name password uid gid gecos home shell)
      (etypecase id
        (string (nix:getpwnam id))
        (integer (nix:getpwuid id)))
    (declare (ignore password))
    (unless (null name)
      (list (cons :name name)
            (cons :user-id uid)
            (cons :group-id gid)
            (cons :gecos gecos)
            (cons :home home)
            (cons :shell shell)))))

;;;; Temporary Files
(defun %open-temporary-file/fd-streams (filename element-type external-format)
  (handler-case
      (multiple-value-bind (fd path)
          (nix:mkstemp filename)
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
