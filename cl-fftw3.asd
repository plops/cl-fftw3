;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-fftw3.asd
;;;; Purpose:       ASDF system definition for FFTW3 package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2009
;;;;
;;;; $Id$
;;;;
;;;; This file, part of FFTW3, is Copyright (c) 2009 by Kevin M. Rosenberg
;;;;
;;;; FFTW3 users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:fftw3-system (:use #:asdf #:cl))
(in-package #:fftw3-system)

(defsystem cl-fftw3
    :name "fftw3"
    :author "Kevin M. Rosenberg <kevin@rosenberg.net>"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "LLGPL"
    :depends-on (:kmrcl :cffi)
    :components
    ((:file "package")
     (:file "specials" :depends-on ("package"))
     (:file "common" :depends-on ("specials"))
     (:file "ffi" :depends-on ("common"))
     (:file "transform" :depends-on ("ffi"))
     (:file "wisdom" :depends-on ("ffi"))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-fftw3))))
  (operate 'load-op 'cl-fftw3-tests)
  (operate 'test-op 'cl-fftw3-tests :force t))
