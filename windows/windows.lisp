;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; windows.lisp --- Low-level interface to the windows API.
;;;
;;; Copyright (c) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;; Copyright (c) 2021, Eric Timmons   <eric@timmons.dev>
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

(in-package #:osicat-windows)

(load-foreign-library "Kernel32.dll")

;;; TODO: use ERRNO-WRAPPER.
(defmacro defwinapi (name-and-opts return-type &body params)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name-and-opts)
    `(defcfun (,c-name ,lisp-name :convention :stdcall ,@options) ,return-type
       ,@params)))

;;; Error Handling

(defwinapi ("GetLastError" get-last-error) :uint32)

(defwinapi ("QueryPerformanceCounter" %query-perf-counter) bool
  (count (:pointer large-integer)))

(defun query-performance-counter ()
  (with-foreign-object (ptr 'large-integer)
    (assert (%query-perf-counter ptr))
    (mem-ref ptr 'large-integer)))

;;; Wide string translation.
;;;
;;; Many windows functions have an ASCII version and a Unicode/Wide String
;;; version. We typically want to use the Unicode versions because they have
;;; fewer restrictions. Unfortunately, Windows has standardized on UTF-16 using
;;; :uint32s instead of :chars. To also guard against any other shenanigans,
;;; just use Windows' built in functionality to create wide strings.

(defwinapi ("WideCharToMultiByte" wide-char-to-multi-byte) :int
  (code-page :uint)
  (flags :uint32)
  (wide-char-str :pointer)
  (wide-char :int)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (default-char :pointer)
  (used-default-char :pointer))

(defwinapi ("MultiByteToWideChar" multi-byte-to-wide-char) :int
  (code-page :uint)
  (flags :uint32)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (wide-char-str :pointer)
  (wide-char :int))

(defun string-to-wstring (string)
  (with-foreign-string (foreign-string string :encoding :utf-8)
    ;; Compute the size needed to hold the wide string, then actually do the
    ;; conversion.
    (let* ((num-chars (multi-byte-to-wide-char +cp-utf-8+ 0 foreign-string -1 (null-pointer) 0))
           (wide-string (foreign-alloc :uint16 :count num-chars)))
      (multi-byte-to-wide-char +cp-utf-8+ 0 foreign-string -1 wide-string num-chars)
      wide-string)))

(defun wstring-to-string (wstring)
  (let ((num-bytes (wide-char-to-multi-byte +cp-utf-8+ 0 wstring -1 (null-pointer) 0 (null-pointer) (null-pointer))))
    (with-foreign-object (foreign-string :uchar num-bytes)
      (wide-char-to-multi-byte +cp-utf-8+ 0 wstring -1 foreign-string num-bytes (null-pointer) (null-pointer))
      (foreign-string-to-lisp foreign-string :encoding :utf-8))))

(defmethod translate-to-foreign (string (type wide-string))
  (string-to-wstring string))

(defmethod translate-from-foreign (pointer (type wide-string))
  (wstring-to-string pointer))

(defmethod free-translated-object (pointer (type wide-string) param)
  (declare (ignore param))
  (foreign-free pointer))

;;; readdir/opendir equivalents

(defwinapi ("FindFirstFileW" find-first-file-w) search-handle
  (file-name wide-string)
  (find-file-data :pointer))

(defwinapi ("FindNextFileW" find-next-file-w) :bool
  (find-file search-handle)
  (find-file-data :pointer))

(defwinapi ("FindClose" find-close) :bool
  (find-file search-handle))

(define-c-struct-wrapper find-data ())

(defmethod print-object ((object find-data) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (find-data-file-name object) (find-data-file-attributes object))))

(defun find-first-file (path)
  "Calls FindFirstFileW with the PATH. Returns one or two VALUES.

If there are no matches for PATH, then NIL is returned as the only value.

If there are matches, a FIND-DATA instance containing the first match is the
first value and a handle for subsequent use with FIND-NEXT-FILE is the second
value.

When finished, the handle must be passed to FIND-CLOSE."
  (with-foreign-object (buf '(:struct find-data))
    (let ((handle (find-first-file-w path buf)))
      (unless (= (pointer-address handle) +invalid-handle-value+)
        (values (make-instance 'find-data :pointer buf) handle)))))

(defun find-next-file (handle)
  "Calls FindNextFileW to continue the search represented by HANDLE. Returns a
FIND-DATA instance or NIL."
  (with-foreign-object (buf '(:struct find-data))
    (let ((result (find-next-file-w handle buf)))
      (if result
          (make-instance 'find-data :pointer buf)
          (let ((code (get-last-error)))
            (unless (= code +error-no-more-files+)
              (error "Error code ~A" code)))))))
