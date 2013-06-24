;;;; Triple DES

(in-package #:des)

(defclass des3 ()
  ((cipher-keys   :type (simple-array (unsigned-byte 64) 3) :initarg :cipher-keys)
   (expanded-keys :type (simple-array * 3))))

(defmethod initialize-instance :after ((o des3) &rest initargs)
  (declare (ignore initargs))
  (with-slots (expanded-keys cipher-keys) o
    (setf expanded-keys (make-array 3))

    (loop with ek = (make-array 16 :element-type '(unsigned-byte 48))
	  for j below 3 do
	    (loop for i from 1 to 16 do
	      (setf (aref ek (1- i)) (ks (aref cipher-keys j) i)))
	    (setf (aref expanded-keys j) ek))))
	      
(defmethod encrypt ((o des3) n)
  "DES3 encryption of 64 bit number N."
  (with-slots (expanded-keys) o
    (let ((w (initial-permutation n)))
      (declare (type (unsigned-byte 64) w))
      (setf w (%encrypt w (aref expanded-keys 0))
	    w (%decrypt w (aref expanded-keys 1))
	    w (%encrypt w (aref expanded-keys 2)))
      (inverse-initial-permutation w))))

(defmethod decrypt ((o des3) n)
  "DES3 decryption of 64 bit number N."
  (with-slots (expanded-keys) o
    (let ((w (initial-permutation n)))
      (declare (type (unsigned-byte 64) w))
      (setf w (%decrypt w (aref expanded-keys 2))
	    w (%encrypt w (aref expanded-keys 1))
	    w (%decrypt w (aref expanded-keys 0))) 
      (inverse-initial-permutation w))))
