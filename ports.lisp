;; Copyright (c) 2005 Nikodemus Siivola, Julian Squires
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

#+sbcl
(progn
  (defun make-fd-stream (fd &key direction element-type external-format)
    (let ((in-p (member direction '(:io :input)))
	  (out-p (member direction '(:io :output))))
      (sb-sys:make-fd-stream fd :input in-p :output out-p
			     :element-type element-type
			     :external-format external-format)))
  (pushnew 'fd-streams *features*))

#+cmu
(progn
  (defun make-fd-stream (fd &key direction element-type external-format)
    (let ((in-p (member direction '(:io :input)))
	  (out-p (member direction '(:io :output))))
      (sys:make-fd-stream fd :input in-p :output out-p
			  :element-type element-type
			  :external-format external-format)))
  (pushnew 'fd-streams *features*))

;; FIXME: This code would work for OpenMCL, except that the FD-STREAM
;; returned by ccl::make-fd-stream is apparently not a stream (as per
;; STREAMP etc).  I'm sure there's something we can do to correct
;; this, but until then, I'm leaving it out.
#+nil ;; openmcl
(progn
  ;; KLUDGE: This is kind of evil, because MAKE-FD-STREAM isn't
  ;; exported from CCL in OpenMCL.  However, it seems to have been
  ;; around for a while, and I'm going to ask the OpenMCL developers
  ;; if they'll add it to the exported interface.
  (defun make-fd-stream (fd &key direction element-type external-format)
    (declare (ignore external-format))
    (ccl::make-fd-stream fd :direction direction :element-type element-type))
  (pushnew 'fd-streams *features*))
