;;; Package definitions

(defpackage :des
  (:use #:cl)
  (:export #:encrypt #:decrypt
	   #:encrypt3 #:decrypt3
	   #:block-encrypt-ecb #:block-decrypt-ecb
	   #:block-encrypt-cbc #:block-decrypt-cbc)
  (:documentation "Data Encryption Standard (DES) encryption and decryption."))

(defpackage :des-tests
  (:use #:cl)
  (:documentation "DES tests"))
