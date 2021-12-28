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
  call-with-directory-iterator    ; OPENDIR and READDIR are unavailable
  mapdir                          ; CALL-WITH-DIRECTORY-ITERATOR is unavailable
  list-directory                  ; CALL-WITH-DIRECTORY-ITERATOR is unavailable
  walk-directory                  ; MAPDIR is unavailable
  delete-directory-and-files      ; WALK-DIRECTORY is unavailable
  read-link                       ; READLINK is unavailable
  make-link                       ; LINK and SYMLINK are unavailable
  user-info                       ; GETPWNAM and GETPWUID are unavailable
  )
