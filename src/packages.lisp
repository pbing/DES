;;;; Package definitions

(defpackage :des
  (:use #:common-lisp)
  (:export #:des #:des3
	   #:encrypt #:decrypt
 	   #:block-encrypt-ecb #:block-decrypt-ecb
 	   #:block-encrypt-cbc #:block-decrypt-cbc)
  (:documentation "Data Encryption Standard (DES) encryption and decryption."))
