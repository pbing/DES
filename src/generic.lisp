;;;; Generic methods common to DES and DES3

(in-package #:des)

(defgeneric encrypt (o n));
(defgeneric decrypt (o n));
