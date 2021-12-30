;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; windowsint.lisp --- Grovel definitions for Windows functions
;;;
;;; Copyright (C) 2021, Eric Timmons <eric@timmons.dev>
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

;;; Definitions in this file should match POSIX 1003.1 or some such.
;;; In practice, though, we place here whatever is supported by the
;;; wimpiest of the POSIX systems, Windows.

(include "windows.h")

(in-package #:osicat-windows)

(constant (+max-path+ "MAX_PATH"))
(constant (+invalid-handle-value+ "INVALID_HANDLE_VALUE"))
(constant (+cp-utf-8+ "CP_UTF8"))

(constant (+error-file-not-found+ "ERROR_FILE_NOT_FOUND"))
(constant (+error-no-more-files+ "ERROR_NO_MORE_FILES"))

(bitfield file-attributes
          ((:archive "FILE_ATTRIBUTE_ARCHIVE"))
          ((:compressed "FILE_ATTRIBUTE_COMPRESSED"))
          ((:device "FILE_ATTRIBUTE_DEVICE"))
          ((:directory "FILE_ATTRIBUTE_DIRECTORY"))
          ((:encrypted "FILE_ATTRIBUTE_ENCRYPTED"))
          ((:hidden "FILE_ATTRIBUTE_HIDDEN"))
          ((:integrity-stream "FILE_ATTRIBUTE_INTEGRITY_STREAM")
           :optional t)
          ((:normal "FILE_ATTRIBUTE_NORMAL"))
          ((:not-content-indexed "FILE_ATTRIBUTE_NOT_CONTENT_INDEXED"))
          ((:no-scrub-data "FILE_ATTRIBUTE_NO_SCRUB_DATA")
           :optional t)
          ((:offline "FILE_ATTRIBUTE_OFFLINE"))
          ((:readonly "FILE_ATTRIBUTE_READONLY"))
          ((:recall-on-data-access "FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS")
           :optional t)
          ((:recall-on-open "FILE_ATTRIBUTE_RECALL_ON_OPEN")
           :optional t)
          ((:reparse-point "FILE_ATTRIBUTE_REPARSE_POINT"))
          ((:sparse-file "FILE_ATTRIBUTE_SPARSE_FILE"))
          ((:system "FILE_ATTRIBUTE_SYSTEM"))
          ((:temporary "FILE_ATTRIBUTE_TEMPORARY"))
          ((:virtual "FILE_ATTRIBUTE_VIRTUAL")))

(cstruct file-time "struct _FILETIME"
         (low-date-time "dwLowDateTime" :type :uint32)
         (high-date-time "dwHighDateTime" :type :uint32))

(cstruct find-data "struct _WIN32_FIND_DATAW"
         (file-attributes "dwFileAttributes" :type file-attributes)
         (creation-time "ftCreationTime" :type (:struct file-time))
         (last-access-time "ftLastAccessTime" :type (:struct file-time))
         (last-write-time "ftLastWriteTime" :type (:struct file-time))
         (file-size-high "nFileSizeHigh" :type :uint32)
         (file-size-low "nFileSizeLow" :type :uint32)
         (reserved-0 "dwReserved0" :type :uint32)
         (reserved-1 "dwReserved1" :type :uint32)
         (file-name "cFileName" :type wide-string :count :auto)
         (alternate-file-name "cAlternateFileName" :type wide-string :count 14)
         ;; The Windows documentation mentions the following three fields, but
         ;; the compiler doesn't seem to know about them.
         ;;
         ;; (file-type "dwFileType" :type :uint32)
         ;; (creator-type "dwCreatorType" :type :uint32)
         ;; (finder-flags "wFinderFlags" :type :uint16)
         )

(bitfield symbolic-link-flags
          ((:directory "SYMBOLIC_LINK_FLAG_DIRECTORY"))
          ((:allow-unprivileged-create "SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE")))
