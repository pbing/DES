;;; Package definitions
;;;
;;; $Id$

(defpackage :des
  (:use #:cl)
  (:export #:encrypt #:decrypt
	   #:encrypt3 #:decrypt3
	   #:block-encrypt-ecb #:block-decrypt-ecb)
  (:documentation "Data Encryption Standard (DES) encryption and decryption."))

(defpackage :des-tests
  (:use #:cl)
  (:documentation "DES tests"))
