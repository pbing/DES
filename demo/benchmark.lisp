;;; Benchmarks

(defparameter *block-length* 10000000)
(defparameter *key* (random (expt 2 64)))
(defparameter *iv* (random (expt 2 64)))

(defparameter *plain-text*   (make-array *block-length* :element-type '(unsigned-byte 64)))
(defparameter *cipher-text*  (make-array *block-length* :element-type '(unsigned-byte 64)))
(defparameter *scratch-text* (make-array *block-length* :element-type '(unsigned-byte 64)))

(defun init-text (text)
  (loop for i below (length text) do
    (setf (aref text i) i)))

(defun clear-text (text)
  (loop for i below (length text) do
    (setf (aref text i) 0)))

(defun print-text (text &optional (n 10))
  (setf n (or n (length text)))
  (loop for i below n do
    (format t "~&~16,'0X~%" (aref text i))))

(defun benchmark-encrypt-ecb ()
  (des:block-encrypt-ecb *plain-text* *cipher-text* *key*)
  (values))

(defun benchmark-encrypt-cbc ()
  (des:block-encrypt-cbc *plain-text* *cipher-text* *key* :iv *iv*)
  (values))

#+(or)
(sb-profile:profile des:block-encrypt-ecb
		    des::initial-permutation
		    des::inverse-initial-permutation
		    des::%encrypt 
		    des::%decrypt 
		    des:encrypt 
		    des:decrypt 
		    des::pc-1 des::pc-2 des::ks 
		    des::e des::p des::f)
;;; (sb-profile:report)
;;; (sb-profile:reset)
;;; (sb-profile:unprofile)


;;; (require :sb-sprof)
#+(or)
(sb-sprof:with-profiling (:max-samples 1000 
			  :mode :cpu
			  :report :flat
			  :show-progress t)
  (benchmark-encrypt-ecb))
