;;; Data Encryption Standard (DES) and Triple-DES
;;;
;;; $Id:$

(defsystem des
  :in-order-to ((test-op (test-op des-tests)))
  :components ((:file "packages")
	       (:file "key-scheduler" :depends-on ("packages"))
	       (:file "s-boxes" :depends-on ("packages"))
	       (:file "cipher-function" :depends-on ("packages"))
	       (:file "des" :depends-on ("packages"))
	       (:file "des3" :depends-on ("packages" "des"))))

(defsystem des-tests
  :depends-on (des sb-rt)
  :components ((:file "packages")
	       (:file "test-permutations" :depends-on ("packages"))
	       (:file "test-des" :depends-on ("packages"))
	       (:file "test-des3" :depends-on ("packages"))))

;;; Local Variables:
;;; mode: Lisp
;;; End:
