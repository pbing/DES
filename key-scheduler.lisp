;;; Key scheduler

(in-package #:des)

(defparameter *keys* (make-array '(3 16) :element-type '(unsigned-byte 56)))

(defun pc-1 (n)
  "Permuted choice 1."
  (declare (type (unsigned-byte 64) n))
  (let ((w 0))
    (declare (type (unsigned-byte 56) w))
    (setf w (dpb (ldb (byte 1  7) n) (byte 1 55) w)
	  w (dpb (ldb (byte 1 15) n) (byte 1 54) w)
	  w (dpb (ldb (byte 1 23) n) (byte 1 53) w)
	  w (dpb (ldb (byte 1 31) n) (byte 1 52) w)
	  w (dpb (ldb (byte 1 39) n) (byte 1 51) w)
	  w (dpb (ldb (byte 1 47) n) (byte 1 50) w)
	  w (dpb (ldb (byte 1 55) n) (byte 1 49) w)

	  w (dpb (ldb (byte 1 63) n) (byte 1 48) w)
	  w (dpb (ldb (byte 1  6) n) (byte 1 47) w)
	  w (dpb (ldb (byte 1 14) n) (byte 1 46) w)
	  w (dpb (ldb (byte 1 22) n) (byte 1 45) w)
	  w (dpb (ldb (byte 1 30) n) (byte 1 44) w)
	  w (dpb (ldb (byte 1 38) n) (byte 1 43) w)
	  w (dpb (ldb (byte 1 46) n) (byte 1 42) w)

	  w (dpb (ldb (byte 1 54) n) (byte 1 41) w)
	  w (dpb (ldb (byte 1 62) n) (byte 1 40) w)
	  w (dpb (ldb (byte 1  5) n) (byte 1 39) w)
	  w (dpb (ldb (byte 1 13) n) (byte 1 38) w)
	  w (dpb (ldb (byte 1 21) n) (byte 1 37) w)
	  w (dpb (ldb (byte 1 29) n) (byte 1 36) w)
	  w (dpb (ldb (byte 1 37) n) (byte 1 35) w)

	  w (dpb (ldb (byte 1 45) n) (byte 1 34) w)
	  w (dpb (ldb (byte 1 53) n) (byte 1 33) w)
	  w (dpb (ldb (byte 1 61) n) (byte 1 32) w)
	  w (dpb (ldb (byte 1  4) n) (byte 1 31) w)
	  w (dpb (ldb (byte 1 12) n) (byte 1 30) w)
	  w (dpb (ldb (byte 1 20) n) (byte 1 29) w)
	  w (dpb (ldb (byte 1 28) n) (byte 1 28) w)

	  w (dpb (ldb (byte 1  1) n) (byte 1 27) w)
	  w (dpb (ldb (byte 1  9) n) (byte 1 26) w)
	  w (dpb (ldb (byte 1 17) n) (byte 1 25) w)
	  w (dpb (ldb (byte 1 25) n) (byte 1 24) w)
	  w (dpb (ldb (byte 1 33) n) (byte 1 23) w)
	  w (dpb (ldb (byte 1 41) n) (byte 1 22) w)
	  w (dpb (ldb (byte 1 49) n) (byte 1 21) w)

	  w (dpb (ldb (byte 1 57) n) (byte 1 20) w)
	  w (dpb (ldb (byte 1  2) n) (byte 1 19) w)
	  w (dpb (ldb (byte 1 10) n) (byte 1 18) w)
	  w (dpb (ldb (byte 1 18) n) (byte 1 17) w)
	  w (dpb (ldb (byte 1 26) n) (byte 1 16) w)
	  w (dpb (ldb (byte 1 34) n) (byte 1 15) w)
	  w (dpb (ldb (byte 1 42) n) (byte 1 14) w)

	  w (dpb (ldb (byte 1 50) n) (byte 1 13) w)
	  w (dpb (ldb (byte 1 58) n) (byte 1 12) w)
	  w (dpb (ldb (byte 1  3) n) (byte 1 11) w)
	  w (dpb (ldb (byte 1 11) n) (byte 1 10) w)
	  w (dpb (ldb (byte 1 19) n) (byte 1  9) w)
	  w (dpb (ldb (byte 1 27) n) (byte 1  8) w)
	  w (dpb (ldb (byte 1 35) n) (byte 1  7) w)

	  w (dpb (ldb (byte 1 43) n) (byte 1  6) w)
	  w (dpb (ldb (byte 1 51) n) (byte 1  5) w)
	  w (dpb (ldb (byte 1 59) n) (byte 1  4) w)
	  w (dpb (ldb (byte 1 36) n) (byte 1  3) w)
	  w (dpb (ldb (byte 1 44) n) (byte 1  2) w)
	  w (dpb (ldb (byte 1 52) n) (byte 1  1) w)
	  w (dpb (ldb (byte 1 60) n) (byte 1  0) w))
    w))

(defun pc-2 (n)
  "Permuted choice 2."
  (declare (type (unsigned-byte 56) n))
  (let ((w 0))
    (declare (type (unsigned-byte 48) w))
    (setf w (dpb (ldb (byte 1 42) n) (byte 1 47) w)
	  w (dpb (ldb (byte 1 39) n) (byte 1 46) w)
	  w (dpb (ldb (byte 1 45) n) (byte 1 45) w)
	  w (dpb (ldb (byte 1 32) n) (byte 1 44) w)
	  w (dpb (ldb (byte 1 55) n) (byte 1 43) w)
	  w (dpb (ldb (byte 1 51) n) (byte 1 42) w)

	  w (dpb (ldb (byte 1 53) n) (byte 1 41) w)
	  w (dpb (ldb (byte 1 28) n) (byte 1 40) w)
	  w (dpb (ldb (byte 1 41) n) (byte 1 39) w)
	  w (dpb (ldb (byte 1 50) n) (byte 1 38) w)
	  w (dpb (ldb (byte 1 35) n) (byte 1 37) w)
	  w (dpb (ldb (byte 1 46) n) (byte 1 36) w)

	  w (dpb (ldb (byte 1 33) n) (byte 1 35) w)
	  w (dpb (ldb (byte 1 37) n) (byte 1 34) w)
	  w (dpb (ldb (byte 1 44) n) (byte 1 33) w)
	  w (dpb (ldb (byte 1 52) n) (byte 1 32) w)
	  w (dpb (ldb (byte 1 30) n) (byte 1 31) w)
	  w (dpb (ldb (byte 1 48) n) (byte 1 30) w)

	  w (dpb (ldb (byte 1 40) n) (byte 1 29) w)
	  w (dpb (ldb (byte 1 49) n) (byte 1 28) w)
	  w (dpb (ldb (byte 1 29) n) (byte 1 27) w)
	  w (dpb (ldb (byte 1 36) n) (byte 1 26) w)
	  w (dpb (ldb (byte 1 43) n) (byte 1 25) w)
	  w (dpb (ldb (byte 1 54) n) (byte 1 24) w)

	  w (dpb (ldb (byte 1 15) n) (byte 1 23) w)
	  w (dpb (ldb (byte 1  4) n) (byte 1 22) w)
	  w (dpb (ldb (byte 1 25) n) (byte 1 21) w)
	  w (dpb (ldb (byte 1 19) n) (byte 1 20) w)
	  w (dpb (ldb (byte 1  9) n) (byte 1 19) w)
	  w (dpb (ldb (byte 1  1) n) (byte 1 18) w)

	  w (dpb (ldb (byte 1 26) n) (byte 1 17) w)
	  w (dpb (ldb (byte 1 16) n) (byte 1 16) w)
	  w (dpb (ldb (byte 1  5) n) (byte 1 15) w)
	  w (dpb (ldb (byte 1 11) n) (byte 1 14) w)
	  w (dpb (ldb (byte 1 23) n) (byte 1 13) w)
	  w (dpb (ldb (byte 1  8) n) (byte 1 12) w)

	  w (dpb (ldb (byte 1 12) n) (byte 1 11) w)
	  w (dpb (ldb (byte 1  7) n) (byte 1 10) w)
	  w (dpb (ldb (byte 1 17) n) (byte 1  9) w)
	  w (dpb (ldb (byte 1  0) n) (byte 1  8) w)
	  w (dpb (ldb (byte 1 22) n) (byte 1  7) w)
	  w (dpb (ldb (byte 1  3) n) (byte 1  6) w)

	  w (dpb (ldb (byte 1 10) n) (byte 1  5) w)
	  w (dpb (ldb (byte 1 14) n) (byte 1  4) w)
	  w (dpb (ldb (byte 1  6) n) (byte 1  3) w)
	  w (dpb (ldb (byte 1 20) n) (byte 1  2) w)
	  w (dpb (ldb (byte 1 27) n) (byte 1  1) w)
	  w (dpb (ldb (byte 1 24) n) (byte 1  0) w))
    w))

(defun ks (key i)
  "Key scheduler Ks."
  (declare (type (unsigned-byte 64) key)
	   (type (integer 1 16) i))
  (let* ((pc1 (pc-1 key))
	 (c (ldb (byte 28 28) pc1))
	 (d (ldb (byte 28  0) pc1)))
    (declare (type (unsigned-byte 56) pc1)
	     (type (unsigned-byte 28) c d))
    (ecase i
      (1 (setf c (dpb (ldb (byte 27 0) c) (byte 27 1) (ldb (byte 1 27) c))
	       d (dpb (ldb (byte 27 0) d) (byte 27 1) (ldb (byte 1 27) d))))
      
      (2 (setf c (dpb (ldb (byte 26 0) c) (byte 26 2) (ldb (byte 2 26) c))
	       d (dpb (ldb (byte 26 0) d) (byte 26 2) (ldb (byte 2 26) d))))
      
      (3 (setf c (dpb (ldb (byte 24 0) c) (byte 24 4) (ldb (byte 4 24) c))
	       d (dpb (ldb (byte 24 0) d) (byte 24 4) (ldb (byte 4 24) d))))

      (4 (setf c (dpb (ldb (byte 22 0) c) (byte 22 6) (ldb (byte 6 22) c))
	       d (dpb (ldb (byte 22 0) d) (byte 22 6) (ldb (byte 6 22) d))))

      (5 (setf c (dpb (ldb (byte 20 0) c) (byte 20 8) (ldb (byte 8 20) c))
	       d (dpb (ldb (byte 20 0) d) (byte 20 8) (ldb (byte 8 20) d))))

      (6 (setf c (dpb (ldb (byte 18 0) c) (byte 18 10) (ldb (byte 10 18) c))
	       d (dpb (ldb (byte 18 0) d) (byte 18 10) (ldb (byte 10 18) d))))

      (7 (setf c (dpb (ldb (byte 16 0) c) (byte 16 12) (ldb (byte 12 16) c))
	       d (dpb (ldb (byte 16 0) d) (byte 16 12) (ldb (byte 12 16) d))))

      (8 (setf c (dpb (ldb (byte 14 0) c) (byte 14 14) (ldb (byte 14 14) c))
	       d (dpb (ldb (byte 14 0) d) (byte 14 14) (ldb (byte 14 14) d))))

      (9 (setf c (dpb (ldb (byte 13 0) c) (byte 13 15) (ldb (byte 15 13) c))
	       d (dpb (ldb (byte 13 0) d) (byte 13 15) (ldb (byte 15 13) d))))

      (10 (setf c (dpb (ldb (byte 11 0) c) (byte 11 17) (ldb (byte 17 11) c))
		d (dpb (ldb (byte 11 0) d) (byte 11 17) (ldb (byte 17 11) d))))

      (11 (setf c (dpb (ldb (byte 9 0) c) (byte 9 19) (ldb (byte 19 9) c))
		d (dpb (ldb (byte 9 0) d) (byte 9 19) (ldb (byte 19 9) d))))

      (12 (setf c (dpb (ldb (byte 7 0) c) (byte 7 21) (ldb (byte 21 7) c))
		d (dpb (ldb (byte 7 0) d) (byte 7 21) (ldb (byte 21 7) d))))

      (13 (setf c (dpb (ldb (byte 5 0) c) (byte 5 23) (ldb (byte 23 5) c))
		d (dpb (ldb (byte 5 0) d) (byte 5 23) (ldb (byte 23 5) d))))

      (14 (setf c (dpb (ldb (byte 3 0) c) (byte 4 25) (ldb (byte 25 3) c))
		d (dpb (ldb (byte 3 0) d) (byte 4 25) (ldb (byte 25 3) d))))

      (15 (setf c (dpb (ldb (byte 1 0) c) (byte 1 27) (ldb (byte 27 1) c))
		d (dpb (ldb (byte 1 0) d) (byte 1 27) (ldb (byte 27 1) d))))

      (16 (setf c (dpb (ldb (byte 0 0) c) (byte 0 28) (ldb (byte 28 0) c))
		d (dpb (ldb (byte 0 0) d) (byte 0 28) (ldb (byte 28 0) d)))))
    (pc-2 (dpb c (byte 28 28) d))))

(defun init-keys (key)
  (declare (type (unsigned-byte 64) key))
  (loop for i from 1 to 16 do
    (setf (aref *keys* 0 (1- i)) (ks key i))))

(defun init-keys-3 (key1 key2 key3)
  (declare (type (unsigned-byte 64) key1 key2 key3))
  (loop for i from 1 to 16 do
    (setf (aref *keys* 0 (1- i)) (ks key1 i)
	  (aref *keys* 1 (1- i)) (ks key2 i)
	  (aref *keys* 2 (1- i)) (ks key3 i))))