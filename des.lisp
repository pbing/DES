;;; Data Encryption Standard (DES)
;;;
;;; $Id$

(in-package #:des)

;;; Hoey's clever initial permutation algorithm, from Outerbridge 
;;; (see Schneier p 478)
(declaim (inline initial-permutation))
(defun initial-permutation (n)
  "Inititial permution IP"
  (declare (type (unsigned-byte 64) n))
  (let ((l 0) (r 0) (w 0))
    (declare (type (unsigned-byte 32) l r w)
	     (optimize speed))
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
    (declare (type (unsigned-byte 32) l r w)
	     (optimize speed))
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

;;; Single DES

(defun encrypt (n key)
  "DES encryption of N with KEY."
  (declare (type (unsigned-byte 64) n))
  (loop with ip of-type (unsigned-byte 64) = (initial-permutation n)
	for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (ks key i)))
	finally (return (inverse-initial-permutation (dpb r (byte 32 32) l)))))

(defun decrypt (n key)
  "DES decryption of N with KEY."
  (declare (type (unsigned-byte 64) n))
  (loop with ip of-type (unsigned-byte 64) = (initial-permutation n)
	for i fixnum from 0 to 16
	for l of-type (unsigned-byte 32) = (ldb (byte 32 32) ip) then r
	and r of-type (unsigned-byte 32) = (ldb (byte 32  0) ip) then (logxor l (f r (ks key (- 17 i))))
	finally (return (inverse-initial-permutation (dpb r (byte 32 32) l)))))

;;; TODO Block encryption/decryption
;;; Mode: ECB - Electronic Codebook
;;;       CBC - Cipher Block Chaining
;;;       CFB - Cipher Feed Back
;;;       OFB - Output Feed Back
;;;

(defun block-encrypt-ecb (plain-text cipher-text key)
  (declare (type (array (unsigned-byte 64)) plain-text cipher-text)
	   (type (unsigned-byte 64) key))
  (let* ((n (length plain-text)))
    (loop for i below n do
      (setf (aref cipher-text i) (encrypt (aref plain-text i) key)))
    cipher-text))

(defun block-decrypt-ecb (cipher-text plain-text key)
  (declare (type (array (unsigned-byte 64)) cipher-text plain-text)
	   (type (unsigned-byte 64) key))
  (let* ((n (length cipher-text)))
    (loop for i below n do
      (setf (aref plain-text i) (decrypt (aref cipher-text i) key)))
    plain-text))
