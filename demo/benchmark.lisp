;;; Benchmarks

(defparameter *block-length* 1000000)
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
  (let ((des (make-instance 'des:des :cipher-key *key*)))
    (des:block-encrypt-ecb des *plain-text* *cipher-text*)
    (values)))

(defun benchmark-encrypt-cbc ()
  (let ((des (make-instance 'des:des :cipher-key *key*)))
    (des:block-encrypt-cbc des *plain-text* *cipher-text* :iv *iv*)
    (values)))

(defun benchmark-encrypt3-ecb ()
  (let ((des (make-instance 'des:des3 :cipher-keys (make-array 3 :initial-element *key*))))
    (des:block-encrypt-ecb des *plain-text* *cipher-text*)
    (values)))

(defun benchmark-encrypt3-cbc ()
  (let ((des (make-instance 'des:des3 :cipher-keys (make-array 3 :initial-element *key*))))
    (des:block-encrypt-cbc des *plain-text* *cipher-text* :iv *iv*)
    (values)))

(init-text *plain-text*)

#+(or)
(sb-profile:profile "DES")
;;; (sb-profile:report)
;;; (sb-profile:reset)
;;; (sb-profile:unprofile)


;;; (require :sb-sprof)
#+(or)
(sb-sprof:with-profiling (:max-samples 1000
			  :mode :time
			  :report :flat)
  (benchmark-encrypt-ecb))
