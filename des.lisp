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

;;; TODO Block encryption/decryption
;;; Mode: ECB - Electronic Codebook
;;;       CBC - Cipher Block Chaining
;;;       CFB - Cipher Feed Back
;;;       OFB - Output Feed Back
;;;

(defun block-encrypt-ecb (plain-text cipher-text key)
  (declare (type (array (unsigned-byte 64)) plain-text cipher-text)
	   (type (unsigned-byte 64) key))
  (init-keys key)
  (loop for i below (length plain-text) do
    (setf (aref cipher-text i) (encrypt-1 (aref plain-text i))))
  cipher-text)

(defun block-decrypt-ecb (cipher-text plain-text key)
  (declare (type (array (unsigned-byte 64)) cipher-text plain-text)
	   (type (unsigned-byte 64) key))
  (init-keys key)
  (loop for i below (length cipher-text) do
    (setf (aref plain-text i) (decrypt-1 (aref cipher-text i))))
  plain-text)
