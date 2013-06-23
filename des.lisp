;;;; Single DES

(in-package #:des)

(defclass des ()
  ((cipher-key    :type (unsigned-byte 64) :initarg :cipher-key)
   (expanded-keys :type (simple-array (unsigned-byte 48) 16))))

(defmethod initialize-instance :after ((o des) &rest initargs)
  (declare (ignore initargs))
  (with-slots (expanded-keys cipher-key) o
    (setf expanded-keys (make-array 16 :element-type '(unsigned-byte 48)))

    (loop for i from 1 to 16 do
      (setf (aref expanded-keys (1- i)) (ks cipher-key i)))))

(defmethod encrypt ((o des) n)
  "DES encryption of 64 bit number N."
  (with-slots (expanded-keys) o
    (inverse-initial-permutation (%encrypt (initial-permutation n) expanded-keys))))

(defmethod decrypt ((o des) n)
  "DES decryption of 64 bit number N."
  (with-slots (expanded-keys) o
    (inverse-initial-permutation (%decrypt (initial-permutation n) expanded-keys))))
