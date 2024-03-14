(in-package :cs-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimize-decl*
    '(declare (optimize (speed 3) (safety 0) (debug 0)))))

(defmacro define-input (name type)
  `(progn
     (declaim (ftype (function (input-state) (values (or ,type null)
						     (or input-state null)))
		     ,name))
     (defun ,name (state)
       (declare (type input-state state))
       (let ((i (is-i state))
             (o (is-o state)))
	 (declare (type (vector ,type *) i))
         (if (>= o (length i))
             (values nil nil)
             (let ((elt (aref i o)))
	       (values elt (mkis :i i :o (1+ o)))))))))

(declaim (ftype char-input-parser %char-input))
(defun %char-input (state)
  #.*optimize-decl*
  (declare (type char-input-state state))
  (let ((i (is-i state))
	(o (is-o state))
	(l (is-l state))
	(c (is-c state))
	(f (is-f state)))
    (if (>= o (length i))
	(values nil nil)
	(let ((char (schar i o)))
	  (values char
		  (mkcis :i i :o (1+ o)
			 :l (if (char= char #\Newline) (1+ l) l)
			 :c (if (char= char #\Newline) 1 (1+ c))
			 :f f))))))

(declaim (ftype char-input-parser char-input))
(defun char-input (state)
  (multiple-value-bind (c s) (%char-input state)
    (let ((match (and c (char= c #\\))))
      (if (not match)
          (values c s)
          (multiple-value-bind (nc ns) (%char-input s)
            (let ((match (and nc (char= nc #\Newline))))
              (if match
                  (char-input ns)
                  (values c s))))))))

(declaim (ftype many-item-parser .many))
(defun .many (state parser &optional (acc (makeq t))
			     (min 0) (max #.(1- array-dimension-limit)) (cnt 0))
  (declare (type input-state state)
	   (type basic-parser parser)
	   (type (queue t) acc))
  #+nil(print `(.many (,(type-of acc)
		       ,(typep acc 'simple-array) ,(typep acc '(array t 1))
		       ,(array-has-fill-pointer-p acc) ,(adjustable-array-p acc))))
  (flet ((many-return ()
	   (if (< cnt min)
	       (values nil nil)
	       (values acc state))))
    (multiple-value-bind (e s) (funcall parser state acc)
      (cond ((>= cnt max) (many-return))
	    ((or (null e) (eq state s)) (many-return))
	    (t (.many s parser e min max (1+ cnt)))))))

(declaim (ftype many-char-parser .many-char))
(defun .many-char (state pred &optional (acc (makeq 'character))
                                        (min 0)
                                        (max #.(1- array-dimension-limit))
                                        (cnt 0))
  (declare (type char-input-state state)
           (type (function (character) boolean) pred)
           (type (queue character) acc)
           (type sb-int:index min max cnt))
  (multiple-value-bind (c s) (char-input state)
    (let ((match (and c (funcall pred c))))
      (if (or (>= cnt max) (not match))
          (if (< cnt min)
	      (values nil nil)
	      (values acc state))
	  (.many-char s pred (enq c acc) min max (1+ cnt))))))

(declaim (ftype char-test-parser .char=))
(defun .char= (state char &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (declare (type char-input-state state)
	   (type character char))
  (multiple-value-bind (c s) (char-input state)
    (let ((match (and c (char= c char))))
      (if (not match)
	  (values nil nil)
	  (values (cenq c acc) s)))))

(declaim (ftype char-test-parser .char/=))
(defun .char/= (state char &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (declare (type char-input-state state)
	   (type character char))
  (multiple-value-bind (c s) (char-input state)
    (let ((match (and c (char/= c char))))
      (if (not match)
	  (values nil nil)
	  (values (cenq c acc) s)))))

(declaim (ftype char-test-parser .char-equal))
(defun .char-equal (state char &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (declare (type char-input-state state)
	   (type character char))
  (multiple-value-bind (c s) (char-input state)
    (let ((match (and c (char-equal* c char))))
      (if (not match)
	  (values nil nil)
	  (values (enq c acc) s)))))

(declaim (ftype char-test-parser .char-not-equal))
(defun .char-not-equal (state char &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (declare (type char-input-state state)
	   (type character char))
  (multiple-value-bind (c s) (char-input state)
    (let ((match (and c (not (char-equal* c char)))))
      (if (not match)
	  (values nil nil)
	  (values (enq c acc) s)))))

(declaim (ftype string-test-parser .string=))
(defun .string= (state string &optional (acc (makeq 'character)) (idx 0))
  #.*optimize-decl*
  (declare (type char-input-state state)
	   (type simple-string string)
	   (type (queue character) acc)
	   (type sb-int:index idx))
  (multiple-value-bind (c s) (char-input state)
    (let ((l (length string)))
      (cond
	((> idx l) (values nil nil))
	((= idx l) (values acc state))
	(t
	 (let* ((match (and c (char= c (schar string idx)))))
	   (if (not match)
	       (values nil nil)
	       (.string= s string (enq c acc) (1+ idx)))))))))

(declaim (ftype string-test-parser .string-equal))
(defun .string-equal (state string &optional (acc (makeq 'character)) (idx 0))
  ;;#.*optimize-decl*
  (declare (type char-input-state state)
	   (type simple-string string)
	   (type (queue character) acc)
	   (type sb-int:index idx))
  (multiple-value-bind (c s) (char-input state)
    (let ((l (length string)))
      (cond
	((> idx l) (values nil nil))
	((= idx l) (values acc state))
	(t
	 (let* ((match (and c (char-equal c (schar string idx)))))
	   (if (not match)
	       (values nil nil)
	       (.string-equal s string (enq c acc) (1+ idx)))))))))

(defmacro .any (&rest parsers)
  (when parsers
    (let ((first (car parsers))
          (rest (cdr parsers))
          (elt (gensym))
          (state (gensym)))
      `(multiple-value-bind (,elt ,state) ,first
         (if ,elt
             (values ,elt ,state)
             ,(if rest
                  `(.any ,@rest)
                  `(values nil nil)))))))

(defmacro .do ((&rest bindings) &body body)
  (if (null bindings)
      `(progn ,@body)
      (let* ((first (car bindings))
             (rest (cdr bindings))
	     (nstate (intern (symbol-name 'nstate))))
	(when (atom (car first))
	  (setf first (cons (list (car first) nstate) (cdr first))))
	(destructuring-bind ((val stat) &rest expr) first
	  (let ((parser (caar expr)))
	    `(multiple-value-bind (,val ,stat) ,@expr
               (if (null ,val)
                   (values nil nil)
                   ,(if rest
			`(.do (,@rest) ,@body)
			`(progn ,@body)))))))))

(macrolet
    ((def (name pred)
       `(progn
          (declaim (ftype (counter-parser char-input-state (queue character))
					  ,name))
          (defun ,name (state &optional (acc (makeq 'character))
				(min 0)
				(max #.(1- array-dimension-limit)))
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            (declare (type char-input-state state)
		     (type (queue character) acc))
            (.do (((elt rst) (.many-char state ,pred acc min max)
	           (declare (type (or (queue character) null) elt)
		            (type char-input-state rst))))
	      (values elt rst))))))
  (def .many-space #'space?)
  (def .many-id1st #'id1st?)
  (def .many-idrest #'idrest?)
  (def .many-digit #'dec?)
  (def .many-octal #'oct?)
  (def .many-hex #'hex?)
  (def .many-ctl #'ctl?)
  (def .many-dec1st #'dec1st?))
