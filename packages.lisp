;;;; Package definitions

(defpackage :des
  (:use #:common-lisp)
  (:export #:encrypt #:decrypt
	   #:encrypt3 #:decrypt3
	   #:block-encrypt-ecb #:block-decrypt-ecb
	   #:block-encrypt-cbc #:block-decrypt-cbc)
  (:documentation "Data Encryption Standard (DES) encryption and decryption."))

(defpackage :des-tests
  (:use #:common-lisp)
  (:documentation "DES tests"))
