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

(defpackage :osicat  
  (:use :cl :uffi) 
  (:documentation 
   "Osicat is a lightweight operating system interface for Common Lisp
on Unix-platforms. It is not a POSIX-style API, but rather a simple
lispy accompaniment to the standard ANSI facilities.

Osicat homepage: 

  http://www.common-lisp.net/project/osicat/

Concepts:

 Designated directory

  When a relative pathname designator is used as a directory designator
  it is first resolved agains *default-pathname-default*, and then
  against the current directory. (With MERGE-PATHNAMES in both cases.)")
  (:shadow
   ;; DIRECTORY is used as constant internally. Let's not confuse other packages.
   #:directory)
  (:export
   ;;; Evironment
   #:environment   
   #:environment-variable
   #:makunbound-environment-variable
   ;; Directories
   #:with-directory-iterator
   #:mapdir
   #:delete-directory
   #:current-directory
   ;; Files
   #:file-kind
   ;; Symlinks
   #:read-link
   #:make-link
   ;; Permissions
   #:file-permissions
   ;; Password entries
   #:user-info
   ;; Version info
   #:*osicat-version*
   ;; Pathname utilities
   #:absolute-pathname
   #:absolute-pathname-p
   #:relative-pathname-p
   #:unmerge-pathnames
   ))
