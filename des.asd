;;;; Data Encryption Standard (DES) and Triple-DES

(defsystem des
  :components ((:file "packages")
	       (:file "key-scheduler" :depends-on ("packages"))
	       (:file "s-boxes" :depends-on ("packages"))
	       (:file "cipher-function" :depends-on ("packages"))
	       (:file "des-core" :depends-on ("packages" "key-scheduler"))
	       (:file "des" :depends-on ("packages" "des-core"))
	       (:file "des3" :depends-on ("packages" "des-core"))))

;;; Usage:
;;;   (asdf:load-system :des-tests)
;;;   (sb-rt:do-tests)
(defsystem des-tests
  :depends-on (des sb-rt)
  :components ((:file "packages")
	       (:file "test-permutations" :depends-on ("packages"))
	       (:file "test-des" :depends-on ("packages"))
	       (:file "test-des3" :depends-on ("packages"))))

;;; Local Variables:
;;; mode: Lisp
;;; End:
