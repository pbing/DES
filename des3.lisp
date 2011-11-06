;;; Triple DES

(in-package #:des)

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
