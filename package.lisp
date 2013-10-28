;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for fftw3 package
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

(defpackage #:cl-fftw3
  (:nicknames #:fftw3)
  (:use #:common-lisp)
  (:export
   ;; transform.lisp
   #:fftw-r2hc-1d
   #:make-fftw-r2r-1d-multi
   #:fftw-r2r-1d-multi
   #:hc-to-mag-phase
   #:fftw-hc2r-1d
   #:fftw-r2c-1d
   #:make-fftw-r2c-1d-multi
   #:make-fftw-c2r-1d-multi
   #:destroy-fftw-multi
   #:fftw-r2c-1d-multi
   #:fftw-c2r-1d-multi
   #:complex-to-mag-phase
   #:fftw-c2r-1d
   #:fftw-c-1d

   ;; wisdom.lisp
   #:import-user-wisdom
   #:export-user-wisdom

   ;; specials
   #:+fftw-forward+
   #:+fftw-backward+
   #:+fftw-hc2r+
   #:+fftw-r2hc+

   #:+fftw-measure+
   #:+fftw-destroy-input+
   #:+fftw-unaligned+
   #:+fftw-conservative-memory+
   #:+fftw-exhaustive+
   #:+fftw-preserve-input+
   #:+fftw-patiento+
   #:+fftw-estimate+
   ))
