;;;; Data Encryption Standard (DES) and Triple-DES

(defsystem des
  :in-order-to ((test-op (test-op des-tests)))
  :pathname "src"
  :components ((:file "packages")
	       (:file "key-scheduler" :depends-on ("packages"))
	       (:file "s-boxes" :depends-on ("packages"))
	       (:file "cipher-function" :depends-on ("packages"))
	       (:file "des-core" :depends-on ("packages"))
	       (:file "generic" :depends-on ("packages"))
	       (:file "des" :depends-on ("packages" "des-core"))
	       (:file "des3" :depends-on ("packages" "des-core"))
	       (:file "block" :depends-on ("packages"))))

;;; for test do: (asdf:test-system :des)
(defmethod perform ((o test-op) (s (eql (find-system :des))))
  (funcall (intern "DO-TESTS" "SB-RT"))
  t)

(defsystem des-tests
  :depends-on (des sb-rt)
  :pathname "tests"
  :components ((:file "packages")
	       (:file "permutation-tests" :depends-on ("packages"))
	       (:file "des-tests" :depends-on ("packages"))
	       (:file "des3-tests" :depends-on ("packages"))))

;;; Local Variables:
;;; mode: Lisp
;;; End:
