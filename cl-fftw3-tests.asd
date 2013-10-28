;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fftw3-tests.asd
;;;; Purpose:       ASDF system definitionf for fftw3 testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Jan 2009
;;;;
;;;; $Id$
;;;; *************************************************************************

(defpackage #:fftw3-tests-system
  (:use #:asdf #:cl))
(in-package #:fftw3-tests-system)

(defsystem cl-fftw3-tests
    :depends-on (rt cl-fftw3)
    :components
    ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-fftw3-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

