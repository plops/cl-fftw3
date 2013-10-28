;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          wisdowm.lisp
;;;; Purpose:       Functions for handling FFTW wisdom
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  March 2009
;;;;
;;;; $Id$
;;;;
;;;; This file, part of FFTW3, is Copyright (c) 2009 by Kevin M. Rosenberg
;;;;
;;;; FFTW3 users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:fftw3)

(defun import-user-wisdom ()
  (let ((file-contents (kmrcl:read-file-to-string *user-wisdom-file*)))
    (when (and (stringp file-contents) (plusp (length file-contents)))
      (fftw-import-wisdom-from-string file-contents))))

(defun export-user-wisdom ()
  (let ((str+ptr (fftw-export-wisdom-to-string)))
    (when (probe-file *user-wisdom-file*)
      (delete-file *user-wisdom-file*))
    (with-open-file (out *user-wisdom-file* :direction :output :if-exists :overwrite
                         :if-does-not-exist :create)
      (format out "~A" (first str+ptr)))
    (free (second str+ptr))))


