(in-package :osicat-system)

(defun write-groveler (file constants)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (format f "
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
")))

(unless (boundp '*grovel*)
  (error "No GROVEL hook!"))

(setf *grovel*
      (lambda (c obj lisp)
	(write-groveler c
			'( ;; File types
			  (mode-mask     . S_IFMT)
			  (directory     . S_IFDIR)
			  (char-device   . S_IFCHR)
			  (block-device  . S_IFBLK)
			  (regular-file  . S_IFREG)
			  (symbolic-link . S_IFLNK)
			  (socket        . S_IFSOCK)
			  (pipe          . S_IFIFO)
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
			  (sticky       . S_ISVTX)))
	(and (zerop (run-shell-command "~A ~A -o ~A"
				       *gcc*
				       (namestring c)
				       (namestring obj)))
	     (zerop (run-shell-command "~A > ~A"
				       (namestring obj)
				       (namestring lisp))))))
