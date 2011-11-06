;;; Triple DES

(in-package #:des)

(defun encrypt3 (n key1 key2 key3)
  "Triple DES encryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (init-keys-3 key1 key2 key3)
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%encrypt w 0)
	  w (%decrypt w 1)
	  w (%encrypt w 2)) 
    (inverse-initial-permutation w)))

(defun decrypt3 (n key1 key2 key3)
  "Triple DES decryption of N with KEY1, KEY2 and KEY3."
  (declare (type (unsigned-byte 64) n key1 key2 key3))
  (init-keys-3 key1 key2 key3)
  (let ((w (initial-permutation n)))
    (declare (type (unsigned-byte 64) w))
    (setf w (%decrypt w 0)
	  w (%encrypt w 1)
	  w (%decrypt w 2)) 
    (inverse-initial-permutation w)))
