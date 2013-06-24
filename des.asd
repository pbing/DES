;;;; Data Encryption Standard (DES) and Triple-DES

(defsystem des
  :in-order-to ((test-op (test-op des-tests)))
  :pathname "src"
  :components ((:file "packages")
	       (:file "key-scheduler" :depends-on ("packages"))
	       (:file "s-boxes" :depends-on ("packages"))
	       (:file "cipher-function" :depends-on ("packages"))
	       (:file "des-core" :depends-on ("packages" "key-scheduler"))
	       (:file "generic" :depends-on ("packages"))
	       (:file "des" :depends-on ("packages" "des-core" "generic"))
	       (:file "des3" :depends-on ("packages" "des-core" "generic"))
	       (:file "block" :depends-on ("packages"))))

(defsystem des-tests
  :depends-on (des sb-rt)
  :pathname "tests"
  :components ((:file "permutation-tests")
	       (:file "des-tests")
	       (:file "des3-tests")))

;;; for test do: (asdf:test-system :des)
(defmethod perform ((o test-op) (s (eql (find-system :des))))
  (funcall (intern "DO-TESTS" "SB-RT"))
  t)

;;; Local Variables:
;;; mode: Lisp
;;; End:
