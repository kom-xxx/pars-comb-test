#+nil(in-package :memoize)

(defvar *memoize-fn-table* (make-hash-table))

(defun memo (fn fn-name)
  (print `(,fn ,fn-name))
  (multiple-value-bind (hash exists) (gethash fn-name *memoize-fn-table*)
    (unless exists
      (setf hash (make-hash-table :test #'equal))
      (setf (gethash fn-name *memoize-fn-table*) hash))
    #'(lambda (&rest args)
	(print args)
	(multiple-value-bind (val foundp) (gethash args hash)
	  (print `(,val ,foundp))
	  (if foundp
	      (progn
		(print :cached)
		(values (car val) (cdr val)))
	      (multiple-value-bind (elt state) (apply fn args)
		(setf (gethash args hash) (cons 
		(values elt state)))))))

#+nil(defun memoize (fn-name)
       (print `(memoize>>> ,fn-name))
       (setf (fdefinition fn-name) (memo (fdefinition fn-name) fn-name)))

(defun memoize (fn-name)
  (print `(memoize>>> ,fn-name))
  (setf (symbol-function fn-name) (memo (symbol-function fn-name) fn-name)))

(defmacro defun-memo (fn args &body body)
  "Define a memoized function"
  `(progn
     (memoize (defun ,fn ,args . ,body))
     ',fn))
#|
(in-package :memoize)

(let ((fn-hash (make-hash-table :test #'eq)))
  (cl:defun memo (fn fn-name)
    (let ((fn-props `(,fn . ,(make-hash-table :test #'equalp))))
      (setf (gethash fn-name fn-hash) fn-props)
      (let ((hash (cdr fn-props)))
	(lambda (&rest args)
	  (multiple-value-bind (val found) (gethash args hash)
	    (if found
		(values-list val)
		(let ((val (multiple-value-list (apply fn args))))
		  (setf (gethash args hash) val)
		  (values-list val))))))))

  (cl:defun unmemoize (fn-name)
    (multiple-value-bind (fn-props found) (gethash fn-name fn-hash)
      (if (not found)
	  nil
	  (let ((unmemo-fn (car fn-props)))
	    (setf (symbol-function fn-name) unmemo-fn)
	    (remhash fn-name fn-hash)))))

  (cl:defun dump-fn-hash ()
    (maphash #'(lambda (k v)
		 (format t "~A (~A ~A)~%" k (car v) (cdr v))
		 (maphash #'(lambda (k v)
			      (format t "	~A: ~A~%" k v))
			  (cdr v)))
	     fn-hash))

  (cl:defun reset ()
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (unmemoize k))
	     fn-hash)
    (setf fn-hash (make-hash-table :test #'eq)))

  (cl:defun memoize (fn)
    (setf (symbol-function fn) (memo (symbol-function fn) fn))))


(defmacro defun (fn args &body body)
  `(progn
     (memoize (cl:defun ,fn ,args . ,body))
     ',fn))
|#
