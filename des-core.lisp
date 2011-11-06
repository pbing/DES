;;; Data Encryption Standard (DES) core functions
;;;
;;; Due to 64 bit return types Functions are inlined in order to
;;; reduce consing.

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
(defun %encrypt (ip round)
  "DES encryption of initial permutation data IP at ROUND."
  (declare (type (unsigned-byte 64) ip)
	   (fixnum round))
  (loop for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (aref *keys* round (1- i))))
	finally (return (dpb r (byte 32 32) l))))

(declaim (inline %decrypt))
(defun %decrypt (ip round)
  "DES decryption of N initial permutation data IP at ROUND."
  (declare (type (unsigned-byte 64) ip)
	   (fixnum round))
  (loop for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (aref *keys* round (- 16 i))))
	finally (return (dpb r (byte 32 32) l))))

(declaim (inline encrypt-1))
(defun encrypt-1 (n)
  "DES encryption with pre-calculated keys."
  (declare (type (unsigned-byte 64) n))
  (inverse-initial-permutation (%encrypt (initial-permutation n) 0)))

(declaim (inline decrypt-1))
(defun decrypt-1 (n)
  "DES decryption with pre-calculated keys."
  (declare (type (unsigned-byte 64) n))
  (inverse-initial-permutation (%decrypt (initial-permutation n) 0)))

(declaim (inline encrypt3-1))
(defun encrypt3-1 (n)
  "DES3 encryption with pre-calculated keys."
  (declare (type (unsigned-byte 64) n))
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%encrypt w 0)
	  w (%decrypt w 1)
	  w (%encrypt w 2)) 
    (inverse-initial-permutation w)))

(declaim (inline decrypt3-1))
(defun decrypt3-1 (n)
  "DES3 decryption with pre-calculated keys."
  (declare (type (unsigned-byte 64) n))
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%decrypt w 2)
	  w (%encrypt w 1)
	  w (%decrypt w 0)) 
    (inverse-initial-permutation w)))
