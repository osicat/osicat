;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; windows.lisp --- Record missing high-level functionality
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

(define-unsupported-functions
  user-info                       ; GETPWNAM and GETPWUID are unavailable
  )

;; SBCL's NATIVE-NAMESTRING adds \\?\ if necessary. But I am doubtful all
;; implementations do.
(defun escaped-namestring (pn)
  (let ((native-namestring (native-namestring pn)))
    (if (and (>= (length native-namestring) 4)
             (string= "\\\\?\\" (subseq native-namestring 0 4)))
        native-namestring
        (concatenate 'string "\\\\?\\" native-namestring))))

(defun dir-namestring-for-find (dir)
  (let ((namestring (native-namestring dir)))
    (concatenate 'string
                 "\\\\?\\"
                 namestring
                 (unless (eql #\\ (aref namestring (1- (length namestring)))) "\\")
                 "*.*")))

(defun call-with-directory-iterator (pathspec fun)
  (let* ((dir (absolute-pathname (pathname pathspec)))
         (old-dir (current-directory))
         (search-string (dir-namestring-for-find dir))
         (firstp t))
    (multiple-value-bind (first-data handle)
        (win:find-first-file search-string)
      (labels ((one-iter ()
                 (let ((data (if firstp
                                 ;; Don't want to call FIND-NEXT-FILE if there
                                 ;; is no match...
                                 (prog1 first-data (setf firstp (null first-data)))
                                 (win:find-next-file handle))))
                   (unless (null data)
                     (let ((name (win:find-data-file-name data))
                           (attributes (win:find-data-file-attributes data)))
                       (cond
                         ((member name '("." "..") :test #'string=)
                          (one-iter))
                         ((member :attribute-directory attributes)
                          (make-pathname :directory `(:relative ,name)))
                         (t
                          (let ((dotpos (position #\. name :from-end t)))
                            (if (and dotpos (plusp dotpos))
                                (make-pathname :name (subseq name 0 dotpos)
                                               :type (subseq name (1+ dotpos)))
                                (make-pathname :name name))))))))))
        (unwind-protect
             (let ((*default-pathname-defaults* dir))
               (setf (current-directory) dir)
               (funcall fun #'one-iter))
          (unless (null handle)
            (win:find-close handle))
          (setf (current-directory) old-dir))))))
