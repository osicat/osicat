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

;;;; A simple groveler loosely based on various SBCL grovelers.
;;;; 
;;;; Jargon note: A groveler is a lisp program that writes a C-program
;;;; that writes a lisp program. The purpose of this excercise is to
;;;; extract C-side definitions in a portable manner.

(in-package :osicat-system)

(defun write-groveler (file constants)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (let ((*print-case* :upcase))
      (format f "
#include <stdio.h>
#include <sys/stat.h>

void
defconstant(char* lisp_name, long unix_number)
{
    printf(\"(defconstant %s %ld) ; #x%lx\\n\",
            lisp_name, unix_number, unix_number);
}
		  
int 
main ()
{
      printf (\"(in-package :osicat)\\n\");
")
    (dolist (c constants)
      (format f "~&     defconstant (\"~A\", ~A);~%" (car c) (cdr c)))
    (format f "
      return 0;
}
"))))

(unless (boundp '*grovel*)
  (error "No GROVEL hook!"))

(defvar *grovel*)

(setf *grovel*
      (lambda (c obj lisp)
	(write-groveler c
			'(;; File types
			  ;; OAOOM Warning: these are explicitly listed 
			  ;; in osicat.lisp as well.
			  (mode-mask         . S_IFMT)
			  (directory         . S_IFDIR)
			  (character-device  . S_IFCHR)
			  (block-device      . S_IFBLK)
			  (regular-file      . S_IFREG)
			  (symbolic-link     . S_IFLNK)
			  (socket            . S_IFSOCK)
			  (pipe              . S_IFIFO)
			  ;; Permissions
			  (user-read    . S_IRUSR)
			  (user-write   . S_IWUSR)
			  (user-exec    . S_IXUSR)
			  (group-read   . S_IRGRP)
			  (group-write  . S_IWGRP)
			  (group-exec   . S_IXGRP)
			  (other-read   . S_IROTH)
			  (other-write  . S_IWOTH)
			  (other-exec   . S_IXOTH)
			  (set-user-id  . S_ISUID)
			  (set-group-id . S_ISGID)
			  (sticky       . S_ISVTX)
			  ;; Misc
			  (eof          . EOF)
			  ))
	(and (zerop (run-shell-command "~A ~A -o ~A"
				       *gcc*
				       (namestring c)
				       (namestring obj)))
	     (zerop (run-shell-command "~A > ~A"
				       (namestring obj)
				       (namestring lisp))))))
