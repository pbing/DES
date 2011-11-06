;;; Single DES

(in-package #:des)

(defun encrypt (n key)
  "DES encryption of N with KEY."
  (declare (type (unsigned-byte 64) n))
  (inverse-initial-permutation (%encrypt (initial-permutation n) key)))

(defun decrypt (n key)
  "DES decryption of N with KEY."
  (declare (type (unsigned-byte 64) n))
  (inverse-initial-permutation (%decrypt (initial-permutation n) key)))

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
