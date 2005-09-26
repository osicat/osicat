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

(in-package :osicat)

;;;; TYPES

;; FIXME: These should be groveled as well.
(def-foreign-type :size-t :unsigned-int)
(def-foreign-type :mode-t :unsigned-int)

;;;; FOREIGN GLUE

(def-function ("osicat_mode" c-file-mode) ((name :cstring) (follow-p :int))
  :module "osicat"
  :returning :int)

(def-function ("osicat_getcwd" c-getcwd) ()
  :module "osicat"
  :returning (* :unsigned-char))

(def-function "osicat_dirent_name" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_pwent_name" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_pwent_passwd" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_pwent_uid" ((entry :pointer-void))
  :module "osicat"
  :returning :int)

(def-function "osicat_pwent_gid" ((entry :pointer-void))
  :module "osicat"
  :returning :int)

(def-function "osicat_pwent_gecos" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_pwent_home" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_pwent_shell" ((entry :pointer-void))
  :module "osicat"
  :returning :cstring)

(def-function "osicat_tmpfile" ()
  :module "osicat"
  :returning :int)

;;;; PLAIN POSIX

(def-function "opendir" ((name :cstring))
  :module "osicat"
  :returning :pointer-void)

(def-function "closedir" ((dir :pointer-void))
  :module "osicat"
  :returning :int)

(def-function "readdir" ((dir :pointer-void))
  :module "osicat"
  :returning :pointer-void)

(def-function "rmdir" ((name :cstring))
    :module "osicat"
    :returning :int)

(def-function "getenv" ((name :cstring))
  :module "osicat"
  :returning :cstring)

(def-function "setenv" ((name :cstring) (value :cstring) (replace :int))
  :module "osicat"
  :returning :int)

(def-function "unsetenv" ((name :cstring))
  :module "osicat"
  :returning :int)

(def-foreign-type cstring-ptr (* :cstring))
(def-foreign-var "environ" 'cstring-ptr "osicat")

(def-function "getpwnam" ((name :cstring))
  :module "osicat"
  :returning :pointer-void)

(def-function "getpwuid" ((id :int))
  :module "osicat"
  :returning :pointer-void)

(def-function "readlink"
    ((name :cstring) (buffer (* :unsigned-char)) (size :size-t))
  :module "osicat"
  :returning :int)

(def-function "symlink" ((old :cstring) (new :cstring))
  :module "osicat"
  :returning :int)

(def-function "link" ((old :cstring) (new :cstring))
  :module "osicat"
  :returning :int)

(def-function "chmod" ((name :cstring) (mode :mode-t))
  :module "osicat"
  :returning :int)

(def-function "chdir" ((name :cstring))
  :module "osicat"
  :returning :int)

(def-function "tmpnam" ((template :cstring))
  :module "osicat"
  :returning :cstring)

(def-function "unlink" ((pathname :cstring))
  :module "osicat"
  :returning :int)
