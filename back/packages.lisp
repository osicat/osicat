;; Copyright (c) 2003 Nikodemus Siivola <nikodemus@random-state.net>
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
   ;; #:run-program
   ;; #:run-shell-command
   ;; #:fork
   ;; #:with-fork
   ))

#||
   ;; Pathnames
   #:os-namestring
   #:realpath
   #:os-directory
   #:default-directory
   #:with-default-directory
   ;; Files
   #:file-attribute-list
   #:file-attribute
   #:probe-directory
   #:probe-link
   #:read-link
   #:directory-list
   #:delete-directory
   #:make-link
   ;; Streams
   #:open-temporary-file
   #:with-temporary-file
   ;; Processes
   ;; #:run-program
   #:program-pid
   #:program-input
   #:program-output
   #:program-error
   #:find-program
   ))
||#