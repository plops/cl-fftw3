(defpackage #:cl-fftw3-tests
  (:nicknames #:fftw3-tests)
  (:use #:cl #:rtest #:cl-fftw3))

(in-package #:cl-fftw3-tests)

(defun double-float-close-p (d1 d2 &key (epsilon 1d-7))
  (when (and (zerop d1) (zerop d2))
    (return-from double-float-close-p t))

  (let* ((delta (abs (- d1 d2)))
         (eps (/ delta (max (abs d1) (abs d2)))))
    (when (or (and (zerop (min (abs d1) (abs d2))) (< (max (abs d1) (abs d2)) epsilon))
              (< eps epsilon))
      t)))

(defun vectors-close-p (v1 v2)
  (unless (eql (length v1) (length v2))
    (return-from vectors-close-p nil))

  (dotimes (i (length v1))
    (unless (and (double-float-close-p (realpart (aref v1 i)) (realpart (aref v2 i)))
                 (double-float-close-p (imagpart (aref v1 i)) (imagpart (aref v2 i))))
      (return-from vectors-close-p nil)))
  t)

(rem-all-tests)

(defparameter *a7* #(0 1 2 3 2 1 0))
(defparameter *a8* #(0 1 2 3 3 2 1 0))
(defparameter *ac7* #(#C(0 0) #C(1 1) #C(2 2) #C(3 3) #C(2 2) #C(1 1) #C(0 0)))
(defparameter *ac8* #(#C(0 0) #C(1 1) #C(2 2) #C(3 3) #C(3 3) #C(2 2) #C(1 1) #C(0 0)))
(defparameter *cc7* #(#C(0 1) #C(1 1) #C(2 2) #C(3 3) #C(2 2) #C(1 1) #C(0 0)))
(defparameter *cc8* #(#C(0 1) #C(1 1) #C(2 2) #C(3 3) #C(3 3) #C(2 2) #C(1 1) #C(0 0)))

(defparameter *b7* #(3 2 1 0 1 2 3))
(defparameter *b8* #(3 2 1 0 0 1 2 3))
(defparameter *bc7* #(#C(3 3) #C(2 2) #C(1 1) #C(0 0) #C(1 1) #C(2 2) #C(3 3)))
(defparameter *bc8* #(#C(3 3) #C(2 2) #C(1 1) #C(0 0) #C(0 0) #C(1 1) #C(2 2) #C(3 3)))

(defparameter *a7hc* (fftw-r2hc-1d *a7*))
(defparameter *a8hc* (fftw-r2hc-1d *a8*))
(defparameter *a7c* (fftw-r2c-1d *a7*))
(defparameter *a8c* (fftw-r2c-1d *a8*))
(defparameter *ac7c* (fftw-c-1d *a7*))
(defparameter *ac8c* (fftw-c-1d *a8*))

(defparameter *a7c-octave* #(#C(1.28571428571428580945d0 0d0)
                             #C(-0.64984533421747225912d0 -0.31294901910963018876d0)
                             #C(0.02743163880429938875d0  0.03439818705768063478d0)
                             #C(-0.02044344744397007946d0 -0.08956859554733599682d0)))

(defparameter *a8c-octave* #(#C(1.50000000000000000000d0 0.00000000000000000000d0)
                             #C(-0.72855339059327373086d0 -0.30177669529663686543d0)
                             #C(0.00000000000000000000d0 0.00000000000000000000d0)
                             #C(-0.02144660940672621363d0 -0.05177669529663689318d0)
                             #C(0.00000000000000000000d0 0.00000000000000000000d0)))

(defparameter *ac7c-octave* #(#C(1.28571428571428580945d0 1.28571428571428580945d0)
                              #C(-0.33689631510784201485d0 -0.96279435332710250339d0)
                              #C(-0.00696654825338124516d0 0.06182982586198002700d0)
                              #C(0.06912514810336592430d0 -0.11001204299130606934d0)
                              #C(-0.11001204299130606934d0 0.06912514810336592430d0)
                              #C(0.06182982586198002700d0 -0.00696654825338124516d0)
                              #C(-0.96279435332710250339d0 -0.33689631510784201485d0)))

(defparameter *cc7c-octave* #(#C(1.28571428571428580945d0 1.42857142857142860315d0)
                              #C(-0.33689631510784201485d0 -0.81993721046995970969d0)
                              #C(-0.00696654825338124516d0 0.20468696871912286928d0)
                              #C(0.06912514810336592430d0 0.03284509986583677293d0)
                              #C(-0.11001204299130606934d0 0.21198229096050877351d0)
                              #C(0.06182982586198002700d0 0.13589059460376159971d0)
                              #C(-0.96279435332710250339d0 -0.19403917225069916563d0)))

(defparameter *ac8c-octave* #(#C(1.50000000000000000000d0 1.50000000000000000000d0)
                              #C(-0.42677669529663686543d0 -1.03033008588991070731d0)
                              #C(0.00000000000000000000d0 0.00000000000000000000d0)
                              #C(0.03033008588991070731d0 -0.07322330470336310682d0)
                              #C(0.00000000000000000000d0 0.00000000000000000000d0)
                              #C(-0.07322330470336310682d0 0.03033008588991070731d0)
                              #C(0.00000000000000000000d0 0.00000000000000000000d0)
                              #C(-1.03033008588991070731d0 -0.42677669529663686543d0)))

(defparameter *cc8c-octave* #(#C(1.50000000000000000000d0 1.62500000000000000000d0)
                              #C(-0.42677669529663686543d0 -0.90533008588991070731)
                              #C(0.00000000000000000000d0 0.12500000000000000000d0)
                              #C(0.03033008588991070731d0 0.05177669529663689318d0)
                              #C(0.00000000000000000000d0 0.12500000000000000000d0)
                              #C(-0.07322330470336310682d0 0.15533008588991070731d0)
                              #C(0.00000000000000000000d0 0.12500000000000000000d0)
                              #C(-1.03033008588991070731d0 -0.30177669529663686543d0)))

(deftest :len.1 (length *a7*) 7)
(deftest :len.2 (length *a8*) 8)
(deftest :len.3 (length *a7hc*) 7)
(deftest :len.4 (length *a8hc*) 8)
(deftest :len.5 (length *a7c*) 4)
(deftest :len.6 (length *a8c*) 5)
(deftest :len.7 (length *b7*) 7)
(deftest :len.8 (length *b8*) 8)

(deftest :eql.1 (vectors-close-p *a7* (fftw-hc2r-1d *a7hc*)) t)
(deftest :eql.2 (vectors-close-p *a8* (fftw-hc2r-1d *a8hc*)) t)
(deftest :eql.3 (vectors-close-p *a7* (fftw-c2r-1d *a7c*)) t)
(deftest :eql.4 (vectors-close-p *a8* (fftw-c2r-1d *a8c*)) t)
(deftest :eql.5 (vectors-close-p *a7* (fftw-c-1d *ac7c* :normalize nil :direction +fftw-backward+)) t)
(deftest :eql.6 (vectors-close-p *a8* (fftw-c-1d *ac8c* :normalize nil :direction +fftw-backward+)) t)

(deftest :eqlc.1 (vectors-close-p
                  *ac7*
                  (fftw-c-1d (fftw-c-1d *ac7* :normalize t :direction +fftw-forward+)
                             :normalize nil :direction +fftw-backward+))
  t)

(deftest :multi.1
    (let* ((multi-r2hc (make-fftw-r2r-1d-multi (length *a7*) +fftw-r2hc+))
           (multi-hc2r (make-fftw-r2r-1d-multi (length *a7*) +fftw-hc2r+))
           (a7hc (copy-seq (fftw-r2r-1d-multi multi-r2hc *a7*)))
           (b7hc (copy-seq (fftw-r2r-1d-multi multi-r2hc *b7*)))
           (a7r (copy-seq (fftw-r2r-1d-multi multi-hc2r a7hc :normalize nil)))
           (b7r (copy-seq  (fftw-r2r-1d-multi multi-hc2r b7hc :normalize nil)))
           (eq (and (vectors-close-p *a7* a7r) (vectors-close-p *b7* b7r) t)))
      (destroy-fftw-multi multi-r2hc)
      (destroy-fftw-multi multi-hc2r)
      eq)
  t)

(deftest :multi.2
    (let* ((multi-r2hc (make-fftw-r2r-1d-multi (length *a8*) +fftw-r2hc+))
           (multi-hc2r (make-fftw-r2r-1d-multi (length *a8*) +fftw-hc2r+))
           (a8hc (copy-seq (fftw-r2r-1d-multi multi-r2hc *a8*)))
           (b8hc (copy-seq (fftw-r2r-1d-multi multi-r2hc *b8*)))
           (a8r (copy-seq (fftw-r2r-1d-multi multi-hc2r a8hc :normalize nil)))
           (b8r (copy-seq  (fftw-r2r-1d-multi multi-hc2r b8hc :normalize nil)))
           (eq (and (vectors-close-p *a8* a8r) (vectors-close-p *b8* b8r) t)))
      (destroy-fftw-multi multi-r2hc)
      (destroy-fftw-multi multi-hc2r)
      eq)
  t)

(deftest :multi.3
    (let* ((multi-r2c (make-fftw-r2c-1d-multi (length *a7*)))
           (a7c (copy-seq (fftw-r2c-1d-multi multi-r2c *a7*)))
           (b7c (copy-seq (fftw-r2c-1d-multi multi-r2c *b7*)))
           (a7 (fftw-c2r-1d a7c))
           (b7 (fftw-c2r-1d b7c))
           (eq (and (vectors-close-p *a7* a7)
                    (vectors-close-p *b7* b7))))
      (destroy-fftw-multi multi-r2c)
      eq)
  t)

(deftest :multi.4
    (let* ((multi-r2c (make-fftw-r2c-1d-multi (length *a8*)))
           (a8c (copy-seq (fftw-r2c-1d-multi multi-r2c *a8*)))
           (b8c (copy-seq (fftw-r2c-1d-multi multi-r2c *b8*)))
           (a8 (fftw-c2r-1d a8c))
           (b8 (fftw-c2r-1d b8c))
           (eq (and (vectors-close-p *a8* a8)
                    (vectors-close-p *b8* b8))))
      (destroy-fftw-multi multi-r2c)
      eq)
  t)

(deftest :multi.5
    (let* ((multi-r2c (make-fftw-r2c-1d-multi (length *a7*)))
           (a7c (copy-seq (fftw-r2c-1d-multi multi-r2c *a7*)))
           (b7c (copy-seq (fftw-r2c-1d-multi multi-r2c *b7*)))
           (multi-c2r (make-fftw-c2r-1d-multi (length a7c)))
           (a7r (copy-seq (fftw-c2r-1d-multi multi-c2r a7c)))
           (b7r (copy-seq (fftw-c2r-1d-multi multi-c2r b7c)))
           (eq (and (vectors-close-p *a7* a7r) (vectors-close-p *b7* b7r) t)))
      (destroy-fftw-multi multi-r2c)
      (destroy-fftw-multi multi-c2r)
      eq)
  t)

(deftest :multi.6
    (let* ((multi-r2c (make-fftw-r2c-1d-multi (length *a8*)))
           (a8c (copy-seq (fftw-r2c-1d-multi multi-r2c *a8*)))
           (b8c (copy-seq (fftw-r2c-1d-multi multi-r2c *b8*)))
           (multi-c2r (make-fftw-c2r-1d-multi (length a8c)))
           (a8r (copy-seq (fftw-c2r-1d-multi multi-c2r a8c)))
           (b8r (copy-seq (fftw-c2r-1d-multi multi-c2r b8c)))
           (eq (and (vectors-close-p *a8* a8r) (vectors-close-p *b8* b8r) t)))
      (destroy-fftw-multi multi-r2c)
      (destroy-fftw-multi multi-c2r)
      eq)
  t)

(deftest :c2c.1
    (let* ((ac7c (fftw-c-1d *ac7*))
           (bc7c (fftw-c-1d *bc7*))
           (cc7c (fftw-c-1d *cc7*))
           (ac7i (fftw-c-1d ac7c :direction +fftw-backward+ :normalize nil))
           (bc7i (fftw-c-1d bc7c :direction +fftw-backward+ :normalize nil))
           (cc7i (fftw-c-1d cc7c :direction +fftw-backward+ :normalize nil))
           (eq (and (vectors-close-p *ac7* ac7i) (vectors-close-p *bc7* bc7i)
                    (vectors-close-p *cc7* cc7i))))
      eq)
  t)

(deftest :c2c.2
    (let* ((ac8c (fftw-c-1d *ac8*))
           (bc8c (fftw-c-1d *bc8*))
           (cc8c (fftw-c-1d *cc8*))
           (ac8i (fftw-c-1d ac8c :direction +fftw-backward+ :normalize nil))
           (bc8i (fftw-c-1d bc8c :direction +fftw-backward+ :normalize nil))
           (cc8i (fftw-c-1d cc8c :direction +fftw-backward+ :normalize nil))
           (eq (and (vectors-close-p *ac8* ac8i) (vectors-close-p *bc8* bc8i)
                    (vectors-close-p *cc8* cc8i))))
      eq)
  t)

(deftest :r2c-octave.1
    (vectors-close-p *a7c* *a7c-octave*) t)

(deftest :r2c-octave.2
    (vectors-close-p *a8c* *a8c-octave*) t)

(deftest :c2c-octave.1
    (vectors-close-p (fftw-c-1d *ac7*) *ac7c-octave*) t)

(deftest :c2c-octave.2
    (vectors-close-p (fftw-c-1d *cc7*) *cc7c-octave*) t)

(deftest :c2c-octave.3
    (vectors-close-p (fftw-c-1d *ac8*) *ac8c-octave*) t)

(deftest :c2c-octave.4
    (vectors-close-p (fftw-c-1d *cc8*) *cc8c-octave*) t)
