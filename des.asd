;;;; Data Encryption Standard (DES) and Triple-DES

(defsystem des
  :in-order-to ((test-op (test-op des-tests)))
  :pathname "src"
  :components ((:file "packages")
	       (:file "key-scheduler")
	       (:file "s-boxes")
	       (:file "cipher-function")
	       (:file "des-core")
	       (:file "generic")
	       (:file "des" :depends-on ("des-core"))
	       (:file "des3" :depends-on ("des-core"))
	       (:file "block")))

(defsystem des-tests
  :depends-on (des sb-rt)
  :pathname "tests"
  :components ((:file "packages")
	       (:file "permutation-tests")
	       (:file "des-tests")
	       (:file "des3-tests")))

;;; for test do: (asdf:test-system :des)
(defmethod perform ((o test-op) (s (eql (find-system :des))))
  (funcall (intern "DO-TESTS" "SB-RT"))
  t)

;;; Local Variables:
;;; mode: Lisp
;;; End:
