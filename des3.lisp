;;; Triple DES

(in-package #:des)

(defun encrypt3 (n key1 key2 key3)
  "Triple DES encryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%encrypt w key1)
	  w (%decrypt w key2)
	  w (%encrypt w key3)) 
    (inverse-initial-permutation w)))

(defun decrypt3 (n key1 key2 key3)
  "Triple DES decryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%decrypt w key1)
	  w (%encrypt w key2)
	  w (%decrypt w key3)) 
    (inverse-initial-permutation w)))
