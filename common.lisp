;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          common.lisp
;;;; Purpose:       Common functions for FFTW3 package
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

(in-package #:fftw3)

(defclass fftw-multi ()
  ((plan :initarg :plan :initform nil :accessor plan)))

(defclass fftw-r2r-1d-multi (fftw-multi)
  ((in-count :initarg :in-count :initform 0 :type fixnum :accessor in-count)
   (out-c :initarg :out-c :initform nil :accessor out-c)
   (in-cf :initarg :in-cf :initform nil :accessor in-cf)
   (out-cf :initarg :out-cf :initform nil :accessor out-cf)))

(defclass fftw-r2c-1d-multi (fftw-multi)
  ((in-count :initarg :in-count :initform 0 :type fixnum :accessor in-count)
   (out-count :initarg :out-count :initform 0 :type fixnum :accessor out-count)
   (complex-output :initarg :complex-output :initform t :type boolean :accessor complex-output)
   (in-cf :initarg :in-cf :initform nil :accessor in-cf)
   (out-cf :initarg :out-cf :initform nil :accessor out-cf)
   (out :initarg :out :initform nil :accessor out)))

(defclass fftw-1d-multi (fftw-multi)
  ((in-count :initarg :in-count :initform 0 :type fixnum :accessor in-count)
   (out-count :initarg :out-count :initform 0 :type fixnum :accessor out-count)
   (in-cf :initarg :in-cf :initform nil :accessor in-cf)
   (out-cf :initarg :out-cf :initform nil :accessor out-cf)
   (out :initarg :out :initform nil :accessor out)))

(defclass fftw-c2r-1d-multi (fftw-multi)
  ((plan-even :initarg :plan-even :initform nil :accessor plan-even)
   (plan-odd :initarg :plan-odd :initform nil :accessor plan-odd)
   (in-count :initarg :in-count :initform 0 :type fixnum :accessor in-count)
   (out-count-odd :initarg :out-count-odd :initform 0 :type fixnum :accessor out-count-odd)
   (out-count-even :initarg :out-count-even :initform 0 :type fixnum :accessor out-count-even)
   (in-cf :initarg :in-cf :initform nil :accessor in-cf)
   (out-cf :initarg :out-cf :initform nil :accessor out-cf)
   (out-odd :initarg :out-odd :initform nil :accessor out-odd)
   (out-even :initarg :out-even :initform nil :accessor out-even)))

(defun normalize-input-range (in start count)
  "Returns the start and count of an input range with range checking."
  (declare #.*standard-optimize-settings*)
  (unless (vectorp in)
    (error "Input needs to be a vector."))
  (when (or (not (integerp start)) (minusp start))
    (error "Start must be an integer, got ~A." start))
  (let ((len (length in)))
    (declare (fixnum len))
    (when (>= start len)
      (error "Start (~D) greater than last element (~D)." start (1- len)))

    (cond
     ((null count)
      (setq count (- len start)))
     ((or (not (integerp count)) (minusp count))
      (error "Count must be positive integer or nil, got ~A." count))
     ((> (+ start count) len)
      (error "Requesting element (~D) past end of vector (~D)." (+ start count) len)))
    (values start count)))

(defun complex-is-even-p (v)
  "Returns T if first and last members have zero imaginary component."
  ;; Do not use *standard-optimize-settings-here* -- they fail on lispworks 5.1.2
  (declare (optimize (speed 3) (space 0)))
  (and (zerop (imagpart (aref v 0)))
       (zerop (imagpart (aref v (1- (length v)))))))

(defun complex-to-mag-phase (cmplx &optional (elements-complex t))
  (declare #.*standard-optimize-settings*)
  (let* ((len (length cmplx))
         (out-len (if elements-complex len (/ len 2)))
         (mag (make-array out-len :element-type 'double-float))
         (phase (make-array out-len :element-type 'double-float)))
    (declare (fixnum len out-len))
    (cond
      (elements-complex
       (dotimes (i out-len)
         (declare (fixnum i))
         (let* ((e (aref cmplx i)))
           (setf (aref mag i) (abs e))
           (setf (aref phase i) (phase e)))))
      (t
       (dotimes (i out-len)
         (declare (fixnum i))
         (let* ((base (+ i i))
                (r (aref cmplx base))
                (i (aref cmplx (1+ base))))
           (setf (aref mag i) (sqrt (+ (* r r) (* i i))))
           (setf (aref phase i) (atan i r))))))
    (values mag phase)))

(defun hc-to-mag-phase (hc)
  "Turns half-complex into magniture and phase vectors."
  (declare #.*standard-optimize-settings*)
  (let* ((n (length hc))
         (out-n (1+ (floor n 2)))
         (mag (make-array out-n :element-type 'double-float))
         (phase (make-array out-n :element-type 'double-float)))
    (declare (fixnum n out-n))
    (dotimes (i out-n)
      (cond
       ((or (zerop i)
            (and (evenp n) (eql i (1- out-n))))
        (setf (aref mag i) (aref hc (/ i 2)))
        (setf (aref phase i) 0d0))
       (t
        (let ((re (aref hc i))
              (im (aref hc (- n i))))
          (setf (aref mag i) (sqrt (+ (* re re) (* im im))))
          (setf (aref phase i) (atan im re))))))
    (values mag phase)))

