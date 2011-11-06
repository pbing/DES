;;; Single DES

(in-package #:des)

(defun encrypt (n key)
  "DES encryption of N with KEY."
  (declare (type (unsigned-byte 64) n key))
  (init-keys key)
  (encrypt-1 n))

(defun decrypt (n key)
  "DES decryption of N with KEY."
  (declare (type (unsigned-byte 64) n key))
  (init-keys key)
  (decrypt-1 n))

;;; Mode: ECB - Electronic Codebook

(defun block-encrypt-ecb (plain-text cipher-text key)
  "DES encryption with Electronic Codebook (ECB) mode of array
PLAN-TEXT with key KEY. Result is array CIPHER-TEXT."
  (declare (type (array (unsigned-byte 64)) plain-text cipher-text)
	   (type (unsigned-byte 64) key))
  (init-keys key)
  (loop for i below (length plain-text) do
    (setf (aref cipher-text i) (encrypt-1 (aref plain-text i))))
  cipher-text)

(defun block-decrypt-ecb (cipher-text plain-text key)
  "DES decryption with Electronic Codebook (ECB) mode of array
CIPHER-TEXT with key KEY. Result is array PLAN-TEXT."
  (declare (type (array (unsigned-byte 64)) cipher-text plain-text)
	   (type (unsigned-byte 64) key))
  (init-keys key)
  (loop for i below (length cipher-text) do
    (setf (aref plain-text i) (decrypt-1 (aref cipher-text i))))
  plain-text)

;;; Mode: CBC - Cipher Block Chaining

(defun block-encrypt-cbc (plain-text cipher-text key &key (iv 0))
  "DES encryption with Cipher Block Chaining (CBC) mode of array
PLAN-TEXT with key KEY and initialization vector IV. Result is array
CIPHER-TEXT."
  (declare (type (array (unsigned-byte 64)) plain-text cipher-text)
	   (type (unsigned-byte 64) key iv))
  (init-keys key)
  (loop for i below (length plain-text)
	for kv = iv then ct
	for pt = (aref plain-text i)
	for ct = (encrypt-1 (logxor pt kv))
	do (setf (aref cipher-text i) ct))
  cipher-text)

(defun block-decrypt-cbc (cipher-text plain-text key &key (iv 0))
  "DES decryption with Cipher Block Chaining (CBC) mode of array
CIPHER-TEXT with key KEY and initialization vector IV. Result is array
PLAN-TEXT."
  (declare (type (array (unsigned-byte 64)) plain-text cipher-text)
	   (type (unsigned-byte 64) key iv))
  (init-keys key)
  (loop for i below (length cipher-text)
	for kv = iv then ct
	for ct = (aref cipher-text i)
	for pt = (logxor (decrypt-1 ct) kv)
	do (setf (aref plain-text i) pt))
  plain-text)

;;; TODO
;;; Mode: CFB - Cipher Feed Back
;;; Mode: OFB - Output Feed Back
