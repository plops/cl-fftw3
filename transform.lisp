;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          transform.lisp
;;;; Purpose:       Transformation functions
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

(defconstant +size-double+ 8)

(defun fftw-r2hc-1d (in &key (start 0) (count nil) (normalize t))
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (start count) (normalize-input-range in start count)
    (declare (fixnum start count))

    (let ((in-pos start)
          (out-c (make-array count :element-type 'double-float))
          (in-cf (fftw-malloc (* count +size-double+)))
          (out-cf (fftw-malloc (* count +size-double+))))
      (declare (fixnum in-pos))
      (dotimes (i count)
        (declare (fixnum i))
        (setf (cffi:mem-aref in-cf :double i) (coerce (aref in in-pos) 'double-float))
        (incf in-pos))

      (let ((p (fftw-plan-r2r-1d count in-cf out-cf +fftw-r2hc+
                                 (logior +fftw-estimate+ +fftw-destroy-input+))))
        (fftw-execute p)
        (fftw-destroy-plan p))

      (if normalize
          (let ((factor (coerce (/ 1 count) 'double-float)))
            (declare (double-float factor))
            (dotimes (i count)
              (declare (fixnum i))
              (setf (aref out-c i) (* factor (cffi:mem-aref out-cf :double i)))))
          (dotimes (i count)
            (declare (fixnum i))
            (setf (aref out-c i) (cffi:mem-aref out-cf :double i))))

      (fftw-free in-cf)
      (fftw-free out-cf)

      out-c)))

(defun make-fftw-r2r-1d-multi (count type)
  (declare #.*standard-optimize-settings*)
  (declare (fixnum count type))
  (let* ((out-c (make-array count :element-type 'double-float))
         (in-cf (fftw-malloc (* count +size-double+)))
         (out-cf (fftw-malloc (* count +size-double+)))
         (plan (fftw-plan-r2r-1d count in-cf out-cf type
                                 (logior +fftw-estimate+ +fftw-destroy-input+))))
    (make-instance 'fftw-r2r-1d-multi
                   :in-count count :out-c out-c :plan plan
                   :in-cf in-cf :out-cf out-cf)))

(defun fftw-r2r-1d-multi (multi in &key (start 0) (count nil) (normalize t))
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (start count) (normalize-input-range in start count)
    (declare (fixnum start count))
    (unless (equal (in-count multi) count)
      (error "Different plan and vector lengths."))

    (let ((in-cf (in-cf multi))
          (out-cf (out-cf multi))
          (out-c (out-c multi))
          (in-pos start))
      (declare (fixnum in-pos))

      (dotimes (i count)
        (declare (fixnum i))
        (setf (cffi:mem-aref in-cf :double i) (coerce (aref in in-pos) 'double-float))
        (incf in-pos))

      (fftw-execute (plan multi))
      (if normalize
          (let ((factor (coerce (/ 1 count) 'double-float)))
            (declare (double-float factor))
            (dotimes (i count)
              (declare (fixnum i))
              (setf (aref out-c i) (* factor (cffi:mem-aref out-cf :double i)))))
          (dotimes (i count)
            (declare (fixnum i))
            (setf (aref out-c i) (cffi:mem-aref out-cf :double i))))

      out-c)))


(defun fftw-hc2r-1d (in)
  (declare #.*standard-optimize-settings*)
  (let* ((count (length in))
         (out-c (make-array count :element-type 'double-float))
         (in-cf (fftw-malloc (* count +size-double+)))
         (out-cf (fftw-malloc (* count +size-double+))))
    (declare (fixnum count))

    (dotimes (i count)
      (declare (fixnum i))
      (setf (cffi:mem-aref in-cf :double i) (aref in i)))

    (let ((p (fftw-plan-r2r-1d count in-cf out-cf +fftw-hc2r+
                               (logior +fftw-estimate+ +fftw-destroy-input+))))
      (fftw-execute p)
      (fftw-destroy-plan p))

    (dotimes (i count)
      (declare (fixnum i))
      (setf (aref out-c i) (cffi:mem-aref out-cf :double i)))
    (fftw-free in-cf)
    (fftw-free out-cf)
    out-c))

(defun fftw-r2c-1d (in &key (start 0) (count nil) (normalize t))
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (start count) (normalize-input-range in start count)
    (declare (fixnum start count))

    (let* ((out-n (1+ (floor count 2)))
           (in-cf (fftw-malloc (* count +size-double+)))
           (out-cf (fftw-malloc (* 2 out-n +size-double+)))
           (in-pos start))
      (declare (fixnum in-pos out-n))

      (dotimes (i count)
        (declare (fixnum i))
        (setf (cffi:mem-aref in-cf :double i) (coerce (aref in in-pos) 'double-float))
        (incf in-pos))

      (let ((p (fftw-plan-dft-r2c-1d count in-cf out-cf
                                     (logior +fftw-estimate+ +fftw-destroy-input+))))
        (fftw-execute p)
        (fftw-destroy-plan p))

      (let ((out (make-array out-n :element-type '(or rational complex)))
            (opos 0))
        (declare (fixnum opos))
        (if normalize
            (let ((factor (coerce (/ 1 count) 'double-float)))
              (declare (double-float factor))
              (dotimes (i out-n)
                (declare (fixnum i))
                (let ((impos (1+ opos)))
                  (declare (fixnum impos))
                  (setf (aref out i) (complex (* (cffi:mem-aref out-cf :double opos) factor)
                                            (* (cffi:mem-aref out-cf :double impos) factor)))
                  (incf opos 2))))
            (dotimes (i out-n)
              (declare (fixnum i))
              (let ((impos (1+ opos)))
                (declare (fixnum impos))
                (setf (aref out i) (complex (cffi:mem-aref out-cf :double opos)
                                            (cffi:mem-aref out-cf :double impos)))
                (incf opos 2))))
        (fftw-free in-cf)
        (fftw-free out-cf)
        out))))

(defun make-fftw-r2c-1d-multi (count &key (complex-output t))
  (declare #.*standard-optimize-settings*)
  (declare (fixnum count))
  (let* ((out-n (1+ (floor count 2)))
         (in-cf (fftw-malloc (* count +size-double+)))
         (out-cf (fftw-malloc (* 2 out-n +size-double+)))
         (plan (fftw-plan-dft-r2c-1d count in-cf out-cf
                                     (logior +fftw-estimate+ +fftw-destroy-input+))))
    (declare (fixnum out-n))

    (unless complex-output
      (incf out-n out-n))

    (make-instance 'fftw-r2c-1d-multi
                   :in-count count :out-count out-n :plan plan
                   :in-cf in-cf
                   :out-cf out-cf
                   :complex-output complex-output
                   :out
                   (if complex-output
                     (make-array out-n :element-type '(or double-float complex) :initial-element 0d0)
                     (make-array out-n :element-type 'double-float)))))

(defgeneric destroy-fftw-multi (multi))
(defmethod destroy-fftw-multi ((multi fftw-multi))
  (declare #.*standard-optimize-settings*)
  (when (plan multi)
    (fftw-destroy-plan (plan multi))
    (setf (plan multi) nil))
  (dolist (slot '(in-cf out-cf))
    (when (slot-value multi slot)
      (fftw-free (slot-value multi slot))
      (setf (slot-value multi slot) nil)))
  (dolist (slot '(plan-even plan-odd))
    (when (and (slot-exists-p multi slot)
               (slot-value multi slot))
      (fftw-destroy-plan (slot-value multi slot))
      (setf (slot-value multi slot) nil))))


(defun fftw-r2c-1d-multi (multi in &key (start 0) (normalize t))
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (start count) (normalize-input-range in start (in-count multi))
    (declare (fixnum start count))

    (unless (equal (length in) (in-count multi))
      (error "In count of multi plan doesn't equal length of in vector."))

    (let ((in-cf (in-cf multi))
          (out-cf (out-cf multi))
          (out (out multi))
          (in-pos start)
          (opos 0)
          (in-count (in-count multi))
          (out-count (out-count multi)))
      (declare (fixnum in-pos opos in-count out-count))

      (dotimes (i in-count)
        (declare (fixnum i))
        (setf (cffi:mem-aref in-cf :double i) (coerce (aref in in-pos) 'double-float))
        (incf in-pos))

      (fftw-execute (plan multi))

      (setf opos 0)
      (cond
       ((complex-output multi)
        (if normalize
            (let ((factor (coerce (/ 1 count) 'double-float)))
              (declare (double-float factor))
              (dotimes (i out-count)
                (declare (fixnum i))
                (let ((impos (1+ opos)))
                  (declare (fixnum impos))
                  (setf (aref out i) (complex (* (cffi:mem-aref out-cf :double opos) factor)
                                              (* (cffi:mem-aref out-cf :double impos) factor)))
                  (incf opos 2))))
            (dotimes (i out-count)
              (declare (fixnum i))
              (let ((impos (1+ opos)))
                (declare (fixnum impos))
                (setf (aref out i) (complex (cffi:mem-aref out-cf :double opos)
                                            (cffi:mem-aref out-cf :double impos)))
                (incf opos 2)))))
       (t
        (when normalize
          (let ((o2 (* out-count 2))
                (factor (coerce (/ 1 count) 'double-float)))
            (declare (fixnum o2)
                     (double-float factor))
            (dotimes (i o2)
              (declare (fixnum i))
              (setf (aref out i) (* factor (aref out i))))))))
      out)))

(defun fftw-c2r-1d (in)
  (declare #.*standard-optimize-settings*)
  (let* ((count (length in))
         (out-n (if (complex-is-even-p in)
                  (* 2 (1- count))
                  (1+ (* 2 (1- count)))))
         (out-c (make-array out-n :element-type 'double-float))
         (in-cf (fftw-malloc (* 2 count +size-double+)))
         (out-cf (fftw-malloc (* 2 count +size-double+)))
         (pos 0))
    (declare (fixnum count out-n pos))

    (setq pos 0)
    (dotimes (i count)
      (declare (fixnum i))
      (let ((c (aref in i)))
        (declare (complex c))
        (setf (cffi:mem-aref in-cf :double pos) (coerce (realpart c) 'double-float))
        (incf pos)
        (setf (cffi:mem-aref in-cf :double pos) (coerce (imagpart c) 'double-float))
        (incf pos)))

    (let ((p (fftw-plan-dft-c2r-1d out-n in-cf out-cf
                                   (logior +fftw-estimate+ +fftw-destroy-input+))))
      (fftw-execute p)
      (fftw-destroy-plan p))
    (dotimes (i out-n)
      (declare (fixnum i))
      (setf (aref out-c i) (cffi:mem-aref out-cf :double i)))

    (fftw-free in-cf)
    (fftw-free out-cf)
    out-c))

(defun make-fftw-c2r-1d-multi (count)
  (declare #.*standard-optimize-settings*)
  (declare (fixnum count))
  (let* ((out-count-even (* 2 (1- count)))
         (out-count-odd (1+ (* 2 (1- count))))
         (in-cf (fftw-malloc (* 2 count +size-double+)))
         (out-cf (fftw-malloc (* 2 (max out-count-odd out-count-even) +size-double+))) ;; use larger size
         (plan-odd (fftw-plan-dft-c2r-1d out-count-odd in-cf out-cf
                                         (logior +fftw-estimate+ +fftw-destroy-input+)))
         (plan-even (fftw-plan-dft-c2r-1d out-count-even in-cf out-cf +fftw-measure+)))
    (declare (fixnum out-count-even out-count-odd))

    (make-instance 'fftw-c2r-1d-multi
                   :in-count count
                   :plan-even plan-even
                   :plan-odd plan-odd
                   :in-cf in-cf
                   :out-cf out-cf
                   :out-count-even out-count-even
                   :out-count-odd out-count-odd
                   :out-even (make-array out-count-even :element-type 'double-float)
                   :out-odd (make-array out-count-odd :element-type 'double-float))))

(defun fftw-c2r-1d-multi (multi in)
  (declare #.*standard-optimize-settings*)
  (let* ((is-even (complex-is-even-p in))
         (out (if is-even (out-even multi) (out-odd multi)))
         (out-count (if is-even (out-count-even multi) (out-count-odd multi)))
         (plan (if is-even (plan-even multi) (plan-odd multi)))
         (count (in-count multi))
         (in-cf (in-cf multi))
         (out-cf (out-cf multi))
         (pos 0))
    (declare (fixnum count out-count pos))

    (setq pos 0)
    (dotimes (i count)
      (declare (fixnum i))
      (let ((c (aref in i)))
        (setf (cffi:mem-aref in-cf :double pos) (coerce (realpart c) 'double-float))
        (incf pos)
        (setf (cffi:mem-aref in-cf :double pos) (coerce (imagpart c) 'double-float))
        (incf pos)))

    (fftw-execute plan)

    (dotimes (i out-count)
      (declare (fixnum i))
      (setf (aref out i) (cffi:mem-aref out-cf :double i)))

    out))

(defun fftw-c-1d (in &key (direction +fftw-forward+)
                (start 0) (count nil) (normalize t))
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (start count) (normalize-input-range in start count)
    (declare (fixnum start count))

    (let* ((out-n count)
           (out (make-array out-n :element-type '(or rational complex)))
           (in-cf (fftw-malloc (* 2 count +size-double+)))
           (out-cf (fftw-malloc (* 2 out-n +size-double+)))
           (opos 0)
           (ipos 0))
      (declare (fixnum out-n ipos opos))

      (setf opos 0)
      (setf ipos start)
      (dotimes (i count)
        (declare (fixnum i))
        (let ((c (aref in ipos)))
          (incf ipos)
          (setf (cffi:mem-aref in-cf :double opos) (coerce (realpart c) 'double-float))
          (incf opos)
          (setf (cffi:mem-aref in-cf :double opos) (coerce (imagpart c) 'double-float))
          (incf opos)))

      (let ((p (fftw-plan-dft-1d count in-cf out-cf direction
                                 (logior +fftw-estimate+ +fftw-destroy-input+))))
        (fftw-execute p)
        (fftw-destroy-plan p))

      (setf opos 0)
      (if normalize
          (let ((factor (coerce (/ 1 count) 'double-float)))
            (declare (double-float factor))
            (dotimes (i out-n)
              (declare (fixnum i))
              (let ((impos (1+ opos)))
                (declare (fixnum impos))
                (setf (aref out i) (complex (* factor (cffi:mem-aref out-cf :double opos))
                                            (* factor (cffi:mem-aref out-cf :double impos)))))
              (incf opos 2)))
          (dotimes (i out-n)
              (declare (fixnum i))
              (let ((impos (1+ opos)))
                (declare (fixnum impos))
                (setf (aref out i) (complex (cffi:mem-aref out-cf :double opos)
                                            (cffi:mem-aref out-cf :double impos))))
              (incf opos 2)))

      (fftw-free in-cf)
      (fftw-free out-cf)
      out)))
