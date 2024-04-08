(in-package :cs-utils)

(declaim (ftype (function (character character) boolean) char-equal*)
	 (inline char-equal*))
(defun char-equal* (c0 c1)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type character c0 c1))
  (if (and (alpha-char-p c0) (alpha-char-p c1))
      (= (abs (- (char-code c0) (char-code c1))) 32)
      (char= c0 c1)))

(defvar *default-queue-depth* 64)

(declaim (inline makeq))
(defun makeq (&optional (type *))
  (make-array  *default-queue-depth* :element-type type
				     :fill-pointer 0 :adjustable t))

(declaim (inline enq))
(defun enq (e q)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type t e)
	   (type queue q))
  (vector-push-extend e q)
  q)

(declaim (inline cmakeq))
(declaim (ftype (function () (queue character))))
(defun cmakeq ()
  (make-array *default-queue-depth* :element-type 'character
				    :fill-pointer 0 :adjustable t))

(declaim (inline cenq))
(defun cenq (c q)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character c)
	   (type (queue character) q))
  (vector-push-extend c q)
  q)

(declaim (inline strcat))
(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(declaim (inline tail))
(declaim (ftype (function ((queue t)) t) tail))
(defun tail (q)
  (let ((len (length q)))
    (when (> len 0)
      (aref q (1- len)))))

(declaim (ftype (function (string) simple-string) to-simple-string))
(defun to-simple-string (vin)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (vector character *) vin))
  (if (typep vin 'simple-string)
      vin
      (let* ((l (length vin))
	     (vs (sb-ext:array-storage-vector vin)))
	(subseq vs 0 l))))

(declaim (ftype (function (hash-table &optional stream) null) dump-hash))
(defun dump-hash (ht &optional (stream *standard-output*))
  (maphash (lambda (k v) (print `(,k ,v) stream)) ht))
