;;;; Triple DES
;;;;
;;;; Due to 64 bit return types functions are inlined in order to
;;;; reduce consing.

(in-package #:des)

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

(defun encrypt3 (n key1 key2 key3)
  "Triple DES encryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (init-keys-3 key1 key2 key3)
  (encrypt3-1 n))

(defun decrypt3 (n key1 key2 key3)
  "Triple DES decryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (init-keys-3 key1 key2 key3)
  (decrypt3-1 n))
