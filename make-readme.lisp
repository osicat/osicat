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

;;;; Ad-hoc tool to extract documentation for OSICAT

(require :asdf)
(require :osicat)

(with-open-file (*standard-output* "README"
				   :direction :output 
				   :if-exists :rename)
  (let ((syms (loop for sym being each external-symbol in :osicat
		    for doc = (cond (#1=(documentation sym 'function) #1#)
				    (#2=(documentation sym 'variable) #2#))
		    when doc
		    collect (cons (symbol-name sym) doc))))
    (setf syms (sort syms #'string< :key #'car))
    (format t "OSICAT ~{~A~^.~}~%~%" osicat:*osicat-version*)
    (format t "~A~%~%---~%~%" (documentation (find-package :osicat) t))
    (format t "Dictionary:~%~%")
    (dolist (cons syms)
      (format t "~& - ~A~%" (string-downcase (car cons))))
    (format t "~%")
    (dolist (cons syms)
      (format t "---~%~%~A~%~%" (cdr cons)))))
