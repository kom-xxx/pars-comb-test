(in-package :cs-deftype)

(deftype queue (&optional (type '*))
  `(and (vector ,type *) (not simple-array)))

(deftype text-position ()
  `(integer 1 #.(1- array-dimension-limit)))

(defstruct (input-state (:constructor %mkis) (:conc-name is-))
  (i #() :type (vector * *) :read-only t)
  (o 0 :type sb-int:index :read-only t))

(defun simplify-array (array)
  (declare (type (array * 1) array)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (etypecase array
    ((simple-array * 1) array)
    ((array * 1)
     (let ((storage (sb-ext:array-storage-vector array))
	   (len (length array)))
       (subseq storage 0 len)))))

(defun mkis (&key (i #()) (o 0))
  (%mkis :i (simplify-array i) :o o))

(defmacro define-input-state
    (name (type &key (constructor nil ctor-p) (conc-name nil cname-p))
     &body body)
  (let ((ctor (if ctor-p constructor  (format nil "make-~:@(~A~)" name)))
	(%ctor (if ctor-p
		   (intern (format nil "%~:@(~A~)" name))
		   (intern (format nil "make-~:@(~A~)" name))))
	(cname (if cname-p conc-name (format nil "~:@(~A~)-" name)))
	(key-list (loop :for form :in body
			:collect (if (consp form)
				     `(,(car form) ,(cadr form))
				     `(,form nil))))
	(arg-list (loop :for form :in body
			:collect (if (consp form)
				     (intern (symbol-name (car form)) :keyword)
				     (intern (symbol-name form) :keyword))
			:collect (if (consp form) (car form) form))))
    `(progn
       (defstruct (,name (:include input-state
			  (i nil :type (or (simple-array ,type (*)) null)))
			 (:constructor ,%ctor) (:conc-name ,cname))
	 ,@body)
       (defun ,ctor (&key (i nil) (o 0) ,@key-list)
	 (let ((input (simplify-array i)))
	   ,(when (not (eq 'character type))
	      `(when (and (> (length input) 0)
			 (not (typep (aref input 0) ',type)))
		 (error "Element type is not ~A." ',type)))
	   (,%ctor :i input :o o ,@arg-list))))))

(define-input-state char-input-state
    (character :constructor mkcis :conc-name is-)
  (l 1 :type text-position :read-only t)
  (c 1 :type text-position :read-only t)
  (f "" :type string :read-only t))

(defmethod print-object ((obj char-input-state) stream)
  (format stream "#S(CHAR-INPUT-STATE :F ~S :I ~S :O ~D :L ~D :C ~S)"
	  (is-f obj)
	  (if (is-i obj) (subseq (is-i obj) (is-o obj)) NIL)
	  (is-o obj) (is-l obj) (is-c obj)))

(defstruct (token (:constructor make-token))
  (props :none :type keyword)
  (value nil :type t))

(define-input-state token-input-state
    (token :constructor mktis :conc-name is-)
  (n nil :type (or input-state null)))

(defmethod print-object ((state token-input-state) stream)
  (let ((vec (is-i state)))
    (format stream "#S(TOKEN-INPUT-STATE :I (~{~A~^ ~}) :O ~D :N ~A"
	    (loop :for i :from (is-o state) :below (length vec)
		  :collect (token-value (aref vec i)))
	    (is-o state)
	    (is-n state))))

(defstruct (cpp-token (:include token))
  (attr nil :type (or keyword (cons keyword list) null))
  (spaces nil :type boolean)
  (state nil :type (or input-state null))
  (next nil :type (or input-state null))
  (file nil :type (or string null))
  (line 1 :type sb-int:index)
  (colmn 1 :type sb-int:index))

(defmethod print-object ((token cpp-token) stream)
  (format stream "#(CPP-TOKEN :PROPS ~A :VALUE ~S :FILE ~S :LINE ~D :COLMN ~D)"
	  (cpp-token-props token) (cpp-token-value token) (cpp-token-file token)
	  (cpp-token-line token) (cpp-token-colmn token)))

(defstruct (cpp-directive (:include token)))

(define-input-state directive-input
    (cpp-directive :constructor mkdis :conc-name is-))

(deftype parser-ret (&optional (object t) (input 'input-state))
  `(values (or ,object null) (or ,input null)))

(deftype simple-parser (&optional (state 'char-input-state)
				  (queue '(queue character)))
  `(function (,state &optional ,queue)
	     (parser-ret ,queue ,state)))

(deftype basic-parser (&optional (state 'input-state) (object '(queue *)))
  `(function (,state &optional ,object) (parser-ret ,object ,state)))

(deftype char-input-parser ()
  `(function (char-input-state) (parser-ret character char-input-state)))

(deftype counter-parser (&optional (state '(input-state))
			   (object '(queue cpp-token)))
  `(function (,state &optional ,object sb-int:index sb-int:index)
		     (parser-ret ,object ,state)))

(deftype many-item-parser (&optional (state 'input-state) (queue '(queue *)))
  `(function (,state basic-parser &optional
		     ,queue sb-int:index sb-int:index sb-int:index)
	     (parser-ret ,queue ,state)))

(deftype many-char-parser ()
  `(function (char-input-state (function (character) boolean)
			       &optional (queue character)
			                 sb-int:index sb-int:index sb-int:index)
	     (parser-ret (queue character) char-input-state)))

(deftype char-test-parser ()
  `(function (char-input-state character &optional (queue character))
             (parser-ret (queue character)  char-input-state)))

(deftype string-test-parser ()
  `(function (char-input-state string &optional (queue character) sb-int:index)
             (parser-ret (queue character) char-input-state)))


#+nil
(deftype many-ctype-parser ()
  `(function (char-input-state &optional (queue character)
		                          sb-int:index sb-int:index)
	     (parser-ret (queue character) char-input-state)))
#+nil
(defun make-cpp-token (&key (props :none) (value nil) (attr nil)
			 (state nil) (next nil))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%make-cpp-token :props props :value value :attr attr :state state :next next))

(deftype parser ()
  `(or char-input-parser
       many-item-parser
       many-char-parser
       many-ctype-parser
       char-test-parser
       string-test-parser))
