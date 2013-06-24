;;;; Data Encryption Standard (DES) core functions
;;;;
;;;; Due to 64 bit return types functions are inlined in order to
;;;; reduce consing.

(in-package #:des)

;;; Hoey's clever initial permutation algorithm, from Outerbridge
;;; (see Schneier p 478)
(declaim (inline initial-permutation))
(defun initial-permutation (n)
  "Inititial permution IP"
  (declare (type (unsigned-byte 64) n))
  (let ((l 0) (r 0) (w 0))
    (declare (type (unsigned-byte 32) l r w))
    (setf l (ldb (byte 32 32) n)
	  r (ldb (byte 32 0) n)

	  w (logand (logxor (ldb (byte 28 4) l) r) #x0f0f0f0f)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 28 4) 0))

	  w (logand (logxor (ldb (byte 16 16) l) r) #x0000ffff)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 16 16) 0))

	  w (logand (logxor (ldb (byte 30 2) r) l) #x33333333)
	  l (logxor l w)
	  r (logxor r (dpb w (byte 30 2) 0))

	  w (logand (logxor (ldb (byte 24 8) r) l) #x00ff00ff)
	  l (logxor l w)
	  r (logxor r (dpb w (byte 24 8) 0))

	  w (logand (logxor (ldb (byte 31 1) l) r) #x55555555)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 31 1) 0)))
    (dpb l (byte 32 32) r)))

(declaim (inline inverse-initial-permutation))
(defun inverse-initial-permutation (n)
  "Inverse initial permution IPINV"
  (declare (type (unsigned-byte 64) n))
  (let ((l 0) (r 0) (w 0))
    (declare (type (unsigned-byte 32) l r w))
    (setf l (ldb (byte 32 32) n)
	  r (ldb (byte 32 0) n)

	  w (logand (logxor (ldb (byte 31 1) l) r) #x55555555)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 31 1) 0))

	  w (logand (logxor (ldb (byte 24 8) r) l) #x00ff00ff)
	  l (logxor l w)
	  r (logxor r (dpb w (byte 24 8) 0))

	  w (logand (logxor (ldb (byte 30 2) r) l) #x33333333)
	  l (logxor l w)
	  r (logxor r (dpb w (byte 30 2) 0))

	  w (logand (logxor (ldb (byte 16 16) l) r) #x0000ffff)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 16 16) 0))

	  w (logand (logxor (ldb (byte 28 4) l) r) #x0f0f0f0f)
	  r (logxor r w)
	  l (logxor l (dpb w (byte 28 4) 0)))
    (dpb l (byte 32 32) r)))

(declaim (inline %encrypt))
(defun %encrypt (ip keys)
  "DES encryption of initial permutation data IP at ROUND."
  (declare (type (unsigned-byte 64) ip))
  (loop for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (aref keys (1- i))))
	finally (return (dpb r (byte 32 32) l))))

(declaim (inline %decrypt))
(defun %decrypt (ip keys)
  "DES decryption of N initial permutation data IP at ROUND."
  (declare (type (unsigned-byte 64) ip))
  (loop for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (aref keys (- 16 i))))
	finally (return (dpb r (byte 32 32) l))))
