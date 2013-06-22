;;;; Package definitions

(defpackage :des
  (:use #:common-lisp)
  (:export #:encrypt #:decrypt  #:encrypt-1 #:decrypt-1
	   #:encrypt3 #:decrypt3 #:encrypt3-1 #:decrypt3-1
	   #:block-encrypt-ecb #:block-decrypt-ecb
	   #:block-encrypt-cbc #:block-decrypt-cbc)
  (:documentation "Data Encryption Standard (DES) encryption and decryption."))

(defpackage :des-tests
  (:use #:common-lisp)
  (:documentation "DES tests"))
