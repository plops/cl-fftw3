;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Special data declarations for FFTW3 package
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

(defconstant +fftw-forward+ -1)
(defconstant +fftw-backward+ +1)

(defconstant +fftw-r2hc+ 0)
(defconstant +fftw-hc2r+ 1)

(defconstant +fftw-measure+ 0)
(defconstant +fftw-destroy-input+ (ash 1 0))
(defconstant +fftw-unaligned+ (ash 1 1))
(defconstant +fftw-conservative-memory+ (ash 1 2))
(defconstant +fftw-exhaustive+ (ash 1 3))
(defconstant +fftw-preserve-input+ (ash 1 4))
(defconstant +fftw-patiento+ (ash 1 5))
(defconstant +fftw-estimate+ (ash 1 6))

(defparameter *user-wisdom-file*
  (concatenate 'string (namestring (user-homedir-pathname)) ".fftw3-wisdom"))

(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")
