;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ffi.lisp
;;;; Purpose:       CFFI interface for FFTW3 package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2009
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

(cffi:define-foreign-library fftw3
    ((or :darwin :macosx) (:or #P"/opt/local/lib/libfftw3.dylib"
                               #P"/usr/lib/libfftw3.dylib"))
    (:linux (:or "libfftw3.so.3"
	     #P"/usr/lib/libfftw3.so"
	     #P"/usr/local/lib/libfftw3.so"))
  (t (:default "libfftw3")))

(cffi:use-foreign-library fftw3)

(cffi:defcstruct fftw-complex-struct
  "Complex number structure."
  (re :double)
  (im :double))

(cffi:defctype fftw-plan :pointer)

(declaim (inline fftw-plan-dft-1d))
(cffi:defcfun ("fftw_plan_dft_1d" fftw-plan-dft-1d) fftw-plan
  (n :int)
  (in (:pointer fftw-complex-struct))
  (out (:pointer fftw-complex-struct))
  (sign :int)
  (flags :uint))

(declaim (inline fftw-plan-dft-2d))
(cffi:defcfun ("fftw_plan_dft_2d" fftw-plan-dft-2d) fftw-plan
  (n0 :int)
  (n1 :int)
  (in (:pointer fftw-complex-struct))
  (out (:pointer fftw-complex-struct))
  (sign :int)
  (flags :uint))

(declaim (inline fftw-plan-dft-3d))
(cffi:defcfun ("fftw_plan_dft_3d" fftw-plan-dft-3d) fftw-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in (:pointer fftw-complex-struct))
  (out (:pointer fftw-complex-struct))
  (sign :int)
  (flags :uint))

(declaim (inline fftw-plan-dft))
(cffi:defcfun ("fftw_plan_dft" fftw-plan-dft) fftw-plan
  (rank :int)
  (n (:pointer :int))
  (in (:pointer fftw-complex-struct))
  (out (:pointer fftw-complex-struct))
  (sign :int)
  (flags :uint))

(declaim (inline fftw-plan-r2r-1d))
(cffi:defcfun ("fftw_plan_r2r_1d" fftw-plan-r2r-1d) fftw-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftw-plan-r2r-2d))
(cffi:defcfun ("fftw_plan_r2r_2d" fftw-plan-r2r-2d) fftw-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftw-plan-r2r-3d))
(cffi:defcfun ("fftw_plan_r2r_3d" fftw-plan-r2r-3d) fftw-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftw-plan-dft-r2c-1d))
(cffi:defcfun ("fftw_plan_dft_r2c_1d" fftw-plan-dft-r2c-1d) fftw-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftw-plan-dft-r2c-2d))
(cffi:defcfun ("fftw_plan_dft_r2c_2d" fftw-plan-dft-r2c-2d) fftw-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftw-plan-dft-r2c-3d))
(cffi:defcfun ("fftw_plan_dft_r2c_3d" fftw-plan-dft-r2c-3d) fftw-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftw-plan-dft-r2c))
(cffi:defcfun ("fftw_plan_dft_r2c" fftw-plan-dft-r2c) fftw-plan
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftw-plan-dft-c2r-1d))
(cffi:defcfun ("fftw_plan_dft_c2r_1d" fftw-plan-dft-c2r-1d) fftw-plan
    (n :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftw-plan-dft-c2r-2d))
(cffi:defcfun ("fftw_plan_dft_c2r_2d" fftw-plan-dft-c2r-2d) fftw-plan
    (n0 :int)
    (n1 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftw-plan-dft-c2r-3d))
(cffi:defcfun ("fftw_plan_dft_c2r_3d" fftw-plan-dft-c2r-3d) fftw-plan
    (n0 :int)
    (n1 :int)
    (n2 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftw-plan-dft-c2r))
(cffi:defcfun ("fftw_plan_dft_c2r" fftw-plan-dft-c2r) fftw-plan
    (rank :int)
    (n (:pointer :int))
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftw-plan-many-dft))
(cffi:defcfun ("fftw_plan_many_dft" fftw-plan-many-dft) fftw-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer fftw-complex-struct))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer fftw-complex-struct))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (sign :int)
    (flags :uint))

(declaim (inline fftw-plan-many-dft_r2c))
(cffi:defcfun ("fftw_plan_many_dft_r2c" fftw-plan-many-dft-r2c) fftw-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :double))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer fftw-complex-struct))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

(declaim (inline fftw-plan-many-dft_c2r))
(cffi:defcfun ("fftw_plan_many_dft_c2r" fftw-plan-many-dft-c2r) fftw-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer fftw-complex-struct))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :double))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

(declaim (inline fftw-plan-many-dft_r2r))
(cffi:defcfun ("fftw_plan_many_dft_r2r" fftw-plan-many-dft-r2r) fftw-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :double))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :double))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (kind (:pointer :int))
    (flags :uint))


(declaim (inline fftw-malloc))
(cffi:defcfun ("fftw_malloc" fftw-malloc) (:pointer fftw-complex-struct)
  (n :int))

(declaim (inline fftw-execute))
(cffi:defcfun ("fftw_execute" fftw-execute) :void
    (p fftw-plan))

(declaim (inline fftw-destroy-plan))
(cffi:defcfun ("fftw_destroy_plan" fftw-destroy-plan) :void
    (p fftw-plan))

(declaim (inline fftw-free))
(cffi:defcfun ("fftw_free" fftw-free) :void
    (p :pointer))


;;; Wisdom functions

(cffi:defcfun ("fftw_import_wisdom_from_string" fftw-import-wisdom-from-string) :int
  (input-string :string))

(cffi:defcfun ("fftw_export_wisdom_to_string" fftw-export-wisdom-to-string) :string+ptr
  )

(cffi:defcfun free :void
  (ptr :pointer))
