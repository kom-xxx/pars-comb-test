(in-package :c-expr)

(defparameter *xdebug-expression* nil)
(defparameter *debug-expression* nil)
(defparameter *debug-break* nil)

(defun dprint (arg)
  (when *xdebug-expression*
    (print arg)))

(defun ddprint (arg)
  (when *debug-expression*
    (print arg)))

(defstruct (c-expression (:include token) (:conc-name expr-))
  (lhs nil :type (or c-expression cpp-token null))
  (rhs nil :type (or c-expression cpp-token null)))

#-nil
(progn
  (defvar *token-input-state-hash* (make-hash-table :test 'eq))
  (declaim (ftype (function (token-input-state)
			    (values &optional (or cpp-token null)
				    (or token-input-state null)))
		  token-input))
  (defun token-input (state)
    (multiple-value-bind (val found) (gethash state *token-input-state-hash*)
      (if (not found)
	  (multiple-value-bind (r s)
	      (let* ((i (is-i state))
		     (o (is-o state))
		     (n (is-n state))
		     (l (length i)))
		(if (< o l)
		    (values (aref i o) (mktis :i i :o (1+ o) :n n))
		    (values nil state)))
	    (setf (gethash state *token-input-state-hash*) (list r s))
	    (values r s))
	  (let ((v (car val))
		(s (cadr val)))
	    (values v s))))))

(defvar *memoize-fn-table* (make-hash-table))

(defun memo (fn fn-name)
  (multiple-value-bind (hash exists) (gethash fn-name *memoize-fn-table*)
    (unless exists
      (setf hash (make-hash-table :test #'equal))
      (setf (gethash fn-name *memoize-fn-table*) hash))
    #'(lambda (&rest args)
	(multiple-value-bind (val foundp) (gethash args hash)
	  (if foundp
	      (values (car val) (cdr val))
	      (multiple-value-bind (elt state) (apply fn args)
		(setf (gethash args hash) (cons elt state))
		(values elt state)))))))

(defun memoize (fn-name)
  (setf (fdefinition fn-name) (memo (fdefinition fn-name) fn-name)))

(defmacro defun-memo (fn args &body body)
  "Define a memoized function"
  `(progn
     (memoize (defun ,fn ,args . ,body))
     ',fn))

(defmacro define-bin-expr (name op parser)
  (let ((fn-name (intern (format nil "~:@(/$~A~)" (symbol-name name))))
	(rest-fn (intern (format nil "~:@(/$~A-rest~)" (symbol-name name))))
	(prop-name (intern (format nil "~:@(c-expr-~A~)" (symbol-name name))
			   :keyword))
	(op-list (if (symbolp op)
		     `(,op state)
		     (loop :for opc :in op
			   :collect `(,opc state) :into opc-list
			   :finally (return `(.any ,@opc-list))))))
    `(progn
       (defun-memo ,rest-fn (state lhs)
	 (.any (.do ((opc ,op-list)
		     (rhs (progn
			    #+nil(print `(,',rest-fn ,opc))
			    (,parser nstate)))
		     (rest (let ((expr (make-c-expression :props ,prop-name
							  :value opc
							  :lhs lhs)))
			     (setf (expr-value lhs) opc)
			     (setf (expr-rhs lhs) rhs)
			     (,rest-fn nstate expr))))
		 (values rest nstate))
	       (.do ((opc ,op-list)
		     (rhs (progn `(,',rest-fn ,opc)
				 (,parser nstate))))
		 (setf (expr-rhs lhs) rhs)
		 (values lhs nstate))))
       (defun-memo ,fn-name (state)
	 (ddprint `(,',fn-name ,state))
	 (.any (.do ((lhs (,parser state))
		     (rest (let ((expr (make-c-expression :props ,prop-name
							  :lhs lhs)))
			     (,rest-fn nstate expr))))
		 (values rest nstate))
	       (.do ((lhs (,parser state)))
		 (values lhs nstate)))))))

(defmacro define-left-assoc (name parser &optional (sep nil))
  (let ((fn-name (intern (format nil "~:@(/$~A~)" (symbol-name name))))
	(rest-fn (intern (format nil "~:@(/$~A-rest~)" (symbol-name name))))
	(props (intern (format nil "~:@(c-expr-~A~)" (symbol-name name))
		       :keyword)))
    `(progn
       (defun-memo ,rest-fn (state lhs)
	 (.any (.do (,@(when sep `((separater (,sep state))))
		     (rhs (,parser ,(if sep `nstate `state)))
		     (rest (let ((expr (make-c-expression :props ,props
							  :lhs lhs)))
			     (setf (expr-value lhs) ,(if sep `separater nil))
			     (setf (expr-rhs lhs) rhs)
			     (,rest-fn nstate expr))))
		 (values rest nstate))
	       (.do (,@(when sep `((separater (,sep state))))
		     (rhs (,parser ,(if sep `nstate `state))))
		 (setf (expr-rhs lhs) rhs)
		 (values lhs nstate))))
       (defun-memo ,fn-name (state)
	 (ddprint `(,',fn-name ,state))
	 (.any (.do ((lhs (,parser state))
		     (rhs (let ((expr (make-c-expression :props ,props
							 :lhs lhs)))
			    (,rest-fn nstate expr))))
		 (values rhs nstate))
	       (.do ((lhs (,parser state)))
		 (values lhs nstate)))))))

(defmacro define-right-assoc (name parser &optional (sep nil))
  (let ((fn-name (intern (format nil "~:@(/$~A~)" (symbol-name name))))
	(rest-fn (intern (format nil "~:@(/$~A-rest~)" (symbol-name name))))
	(props (intern (format nil "~:@(c-expr-~A~)" (symbol-name name))
		       :keyword)))
    `(progn
       (defun-memo ,rest-fn (state rhs)
	 (.any (.do (,@(when sep `((separater (,sep state))))
		     (val (,parser ,(if sep `nstate `state)))
		     (rest (let ((expr (make-c-expression :props ,props
							  :rhs val)))
			     (,rest-fn nstate expr))))
		 (setf (expr-lhs rhs) rest)
		 (values rhs nstate))
	       (.do (,@(when sep `((separater (,sep state))))
		     (val (,parser ,(if sep `nstate `state))))
		 (let ((expr (make-c-expression :props ,props
						:rhs val)))
		   (setf (expr-lhs rhs) expr)
		   (values rhs nstate)))))
       (defun-memo ,fn-name (state)
	 (ddprint `(,',fn-name ,state))
	 (.any (.do ((lhs (,parser state))
		     (rhs (let ((expr (make-c-expression :props ,props
							 :rhs lhs)))
			    (,rest-fn nstate expr))))
		 (values rhs nstate))
	       (.do ((lhs (,parser state)))
		 (let ((expr (make-c-expression :props ,props
						:rhs lhs)))
		   (values expr nstate))))))))

(defmacro define-c-punct-parser (name)
  (let ((fn-name (intern (format nil "//~A" name)))
	(props (intern (format nil "C-PUNCTUATOR-~A" name) :keyword)))
    `(progn
       (declaim (ftype (function (token-input-state)
				 (values (or cpp-token null)
					 (or token-input-state null)))
		       ,fn-name))
       (defun-memo ,fn-name (state)
	 (dprint `(,',fn-name ,state))
	 (.do ((token (token-input state)))
	   (if (eq (cpp-token-props token) ,props)
	       (values token nstate)
	       (values nil nil)))))))

#.(loop :for (_ . kwd) :in cpp-lex::*c-punctuators-list*
	:collect `(define-c-punct-parser ,kwd) :into parser-list
	:finally (return `(progn ,@parser-list)))

(defmacro define-c-keyword-parser (name)
  (let ((fn-name (intern (format nil "~:@(//~A~)" name)))
	(kwd-name (intern (format nil "~:@(c-keyword-~A~)" name) :keyword)))
    `(progn
       (declaim (ftype (function (token-input-state)
				 (values (or cpp-token null)
					 (or token-input-state null)))
		       ,fn-name))
       (defun-memo ,fn-name (state)
	 (dprint `(,',fn-name ,state))
	 (.do ((token (token-input state)))
	   #+nil
	   (print `(,',fn-name ,token))
	   (let ((props (cpp-token-props token)))
	     (if (eq props ,kwd-name)
		 (values token nstate)
		 (if (eq props :identifier)
		     (multiple-value-bind (props found)
			 (gethash (cpp-token-value token)
				  cpp-lex::*c-keyword-hash*)
		       (if (and found (eq props ,kwd-name))
			   (progn
			     (setf (cpp-token-props token) props)
			     (values token nstate))
			   (values nil nil)))
		     (values nil nil)))))))))

#.(loop :for name :in cpp-lex::*c-keyword-list*
	:collect `(define-c-keyword-parser ,name) :into parser-list
	:finally (return `(progn ,@parser-list)))

(defun-memo //pp-token (state)
  (.do ((token (token-input state)))
    (let ((char-state (mkcis :i (cpp-token-value token))))
      #+nil
      (print `(:**** //pp-token ,(cpp-token-value token) ,char-state))
      (.do (((token xstate) (/pp-token  char-state)
	     (declare (ignore xstate))))
	(values (vector-pop token) nstate)))))

(defun-memo //constant (state)
  (dprint `(//constant ,state))
  (.any (//floating-constant state) (//integer-constant state)
	(//enumeration-constant state) (//character-constant state)))

(defun-memo //integer-constant (state)
  (dprint `(//integer-constant ,state))
  (.do ((token (token-input state)))
    (let ((props (cpp-token-props token)))
      #+nil
      (print `(:+++ //integer-constant ,props ,(cpp-token-value token)))
      #+nil
      (when (string= (cpp-token-value token) "2")
	(break))
      (case props
	(:pp-number
	 (let ((tstate (mkcis :i (cpp-token-value token))))
	   (.do (((integer _) (/integer-constant tstate)
		  (declare (ignore _))))
	     (values (aref integer 0) nstate))))
	(:integer-constant
	 (values token nstate))
	(t
	 (values nil nil))))))

(defun-memo //floating-constant (state)
  (dprint `(//floating-constant ,state))
  (.do ((token (token-input state)))
    (if (eq (cpp-token-props token) :pp-number)
	(let ((tstate (mkcis :i (cpp-token-value token))))
	  (.do (((float _) (/floating-constant tstate)
		 (declare (ignore _))))
	    (values (aref float 0) nstate)))
	(values nil nil))))

(defun-memo //character-constant (state)
  (dprint `(//character-constant ,state))
  (.do ((token (token-input state)))
    (if (eq (cpp-token-props token) :character-constant)
	(values token nstate)
	(values nil nil))))

(defun-memo //identifier (state)
  (dprint `(//identifier ,state))
  (.do ((token (token-input state)))
    (if (eq (cpp-token-props token) :identifier)
	(multiple-value-bind (_ found)
	    (gethash (cpp-token-value token) cpp-lex::*c-keyword-hash*)
	  (declare (ignore _))
	  (if found
	      (values nil nil)
	      (values token nstate)))
	(values nil nil))))

(defun-memo //string-literal (state)
  (dprint `(//string-literal ,state))
  (.do ((token (token-input state)))
    (if (eq (cpp-token-props token) :string-literal)
	(values token nstate)
	(values nil nil))))

(defun-memo //enumeration-constant (state)
  (dprint `(//enumeration-constant ,state))
  (//identifier state))

(defun-memo /$primary-expression (state)
  (ddprint `(/$primary-expression ,state))
  (.do ((expr (.any (//identifier state)
		    (//constant state)
		    (//string-literal state)
		    (.do ((lparen (//lparen state))
			  (expr (/$expression nstate))
			  (rparen (//rparen nstate)))
		      #-nil
		      (let ((expr (make-c-expression
				   :props :c-expr-primary-expression
				   :value lparen
				   :lhs expr)))
			(values expr nstate)))
		    (/$generic-selection state))))
    (values expr nstate)))

(defun-memo /$generic-selection (state)
  (ddprint `(/$generic-selection ,state))
  (.do ((generic (//_generic state))
	(lparen (//lparen nstate))
	(assign (/$assignment-expression nstate))
	(comma (//comma nstate))
	(assoc-iist (/$generic-assoc-list nstate))
	(rparen (//rparen nstate)))
    (let ((expr (make-c-expression :props :c-expr-generic-selection
				   :value generic
				   :lhs assign
				   :rhs assoc-iist)))
      (values expr nstate))))

(define-left-assoc generic-assoc-list /$generic-association //comma)

(defun-memo /$generic-association (state)
  (ddprint `(/$generic-association ,state))
  (.any (.do ((type-name (/$type-name state))
	      (colon (//colon nstate))
	      (assign (/$assignment-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-generic-association
					 :lhs type-name
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((default (//default state))
	      (colon (//colon nstate))
	      (assign (/$assignment-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-generic-association
					 :lhs default
					 :rhs assign)))
	    (values expr nstate)))))

(defun-memo /$%%postfix-expression (state)
  (ddprint `(/$%%postfix-expression ,state))
  (.any (.do ((lbraket (//lbraket state))
	      (lhs (/$expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :value lbraket
					 :lhs lhs)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (arg-list (/$argument-expression-list nstate))
	      (rparen (//rparen nstate)))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :value rparen
					 :lhs arg-list)))
	    (values expr nstate)))
	(.do ((opc (.any (//dot state) (//point-to state)))
	      (ident (//identifier nstate)))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :value opc
					 :lhs ident)))
	    (values expr nstate)))
	(.do ((opc (.any (//incr state) (//decr state))))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :value opc)))
	    (values expr nstate)))))

(define-left-assoc %postfix-expression /$%%postfix-expression)

(defun-memo /$postfix-expression (state)
  (ddprint `(/$postfix-expression ,state))
  (.any (.do ((lhs (/$primary-expression state))
	      (rhs (/$%postfix-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :lhs lhs
					 :rhs rhs)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (type-name (/$type-name nstate))
	      (rparen (//rparen nstate))
	      (lbrace (//lbrace nstate))
	      (init-list (/$initializer-list nstate))
	      (comma (//comma nstate))
	      (rbrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-postfix-expression
					 :lhs type-name
					 :rhs init-list)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
		(type-name (/$type-name nstate))
		(rparen (//rparen nstate))
		(lbrace (//lbrace nstate))
		(init-list (/$initializer-list nstate))
		(rbrace (//rbrace nstate)))
	    (let ((expr (make-c-expression :props :c-expr-postfix-expression
					   :lhs type-name
					   :rhs init-list)))
	      (values expr nstate)))
	(.do ((lhs (/$primary-expression state)))
	  (values lhs nstate))))

(define-left-assoc argument-expression-list /$assignment-expression //comma)

(defun-memo /$unary-expression (state)
  (ddprint `(/$unary-expression ,state))
  (.any (.do ((postfix (/$postfix-expression state)))
	  (values postfix nstate))
	(.do ((opc (.any (//incr state) (//decr state)))
	      (lhs (/$unary-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-unary-expression
					 :value opc
					 :lhs lhs)))
	    (values expr nstate)))
	(.do ((opc (.any (//bitand state) (//mul state) (//plus state)
			 (//minos state) (//bitnot state) (//lognot state)))
	      (lhs (/$cast-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-unary-expression
					 :value opc
					 :lhs lhs)))
	    #+nil
	    (print `(:@@@ /$unary-expression lhs=> ,lhs))
	    (values expr nstate)))
	(.do ((opc (.any (//sizeof state)) (//_alignof state))
	      (lparen (//lparen nstate))
	      (type-name (/$type-name nstate))
	      (rparen (//rparen nstate)))
	  (let ((expr (make-c-expression :props :c-expr-unary-expression
					 :value opc
					 :lhs type-name)))
	    (values expr nstate)))))

(defun-memo /$cast-expression (state)
  (ddprint `(/$cast-expression ,state))
  (.any (.do ((lparen  (//lparen state))
	      (type-name (progn (ddprint `(lparen ,lparen)) (/$type-name nstate)))
	      (rparen (progn (ddprint `(type-name ,type-name)) (//rparen nstate)))
	      (u-expr (progn (ddprint `(rparen ,rparen)) (/$unary-expression nstate))))
	  (let ((expr (make-c-expression :props :c-expr-cast-expression
					 :lhs type-name
					 :rhs u-expr)))
	    (values expr nstate)))
	 (.do ((expr (/$unary-expression state)))
	   (values expr nstate))))

(define-bin-expr
    multiplicative-expression (//mul //div //mod) /$cast-expression)
(define-bin-expr
    additive-expression (//plus //minos) /$multiplicative-expression)
(define-bin-expr shift-expression (//lsh //rsh) /$additive-expression)
(define-bin-expr relational-expression (//gt //lt //ge //le)
  /$shift-expression)
(define-bin-expr equality-expression (//eq //ne) /$relational-expression)
(define-bin-expr bitand-expression //bitand /$equality-expression)
(define-bin-expr bitxor-expression //bitxor /$bitand-expression)
(define-bin-expr bitior-expression //bitior /$bitxor-expression)
(define-bin-expr logand-expression //logand /$bitior-expression)
(define-bin-expr logior-expression //logior /$logand-expression)

(defun-memo /$conditional-expression (state) 
  (ddprint `(/$conditional-expression ,state))
  (.any (.do ((test (/$logior-expression state))
	      (? (//question nstate))
	      (then (/$expression nstate))
	      (colon (//colon nstate))
	      (else (/$conditional-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-conditional-expression
					 :value test
					 :lhs then
					 :rhs else)))
	    (values expr nstate)))
	(.do ((test (/$logior-expression state)))
	  (values test nstate))))

(defun-memo /$assignment-expression (state)
  (ddprint `(/$assignment-expression ,state))
  (.any (.do ((lhs (/$unary-expression state))
	      (opc (.any (//assign nstate) (//mul-assign nstate)
			 (//div-assign nstate) (//mod-assign nstate)
			 (//plus-assign nstate) (//minos-assign nstate)
			 (//lsh-assign nstate) (//rsh-assign nstate)
			 (//bitand-assign nstate) ( //bitxor-assign nstate)
			 (//bitior-assign nstate)))
	      (rhs (/$assignment-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-assignment-expression
					 :value opc
					 :lhs lhs
					 :rhs rhs)))
	    (values expr nstate)))
	(.do ((expr (/$conditional-expression state)))
	  (values expr nstate))))

(define-left-assoc expression /$assignment-expression //comma)

(declaim (inline /$constant-expression))
(defun-memo /$constant-expression (state)
  (ddprint `(/$constant-expression ,state))
  (/$conditional-expression state))

;;;
;;;
;;;
(defun-memo /$declaration (state)
  (ddprint `(/$declaration ,state))
  (.any (.do ((decl-spec (/$declaration-specifiers state))
	      (init-decl-list (/$init-declarator-list nstate)))
	  (let ((expr (make-c-expression :props :c-expr-declaration
					 :lhs decl-spec
					 :rhs init-decl-list)))
	    (values expr nstate)))
	(.do ((decl-spec (/$declaration-specifiers state)))
	  (let ((expr (make-c-expression :props :c-expr-declaration
					 :lhs decl-spec)))
	    (values expr nstate)))
	(.do ((expr (/$static_assert-declaration state)))
	  (let ((expr (make-c-expression :props :c-expr-declaration
					 :lhs expr)))
	    (values expr nstate)))))

(defun-memo /$declaration-specifiers (state)
  (ddprint `(/$declaration-specifiers ,state))
  (.any (.do ((spec (.any (/$storage-class-specifier state)
			  (/$type-specifier state)
			  (/$type-qualifier state)
			  (/$function-specifier state)
			  (/$alignment-specifier state)))
	      (rhs (/$declaration-specifiers nstate)))
	  (let ((expr (make-c-expression :props :c-expr-declaration-specifiers
					 :lhs spec
					 :rhs rhs)))
	    (values expr nstate)))
	(.do ((spec (.any (/$storage-class-specifier state)
			  (/$type-specifier state)
			  (/$type-qualifier state)
			  (/$function-specifier state)
			  (/$alignment-specifier state))))
	  (let ((expr (make-c-expression :props :c-expr-declaration-specifiers
					 :lhs spec)))
	    (values expr nstate)))))

(define-left-assoc init-declarator-list /$init-declarator //comma)

(defun-memo /$init-declarator (state)
  (ddprint `(/$init-declarator ,state))
  (.any (.do ((decl (/$declarator state))
	      (assign (//assign nstate))
	      (init (/$initializer nstate)))
	  (let ((expr (make-c-expression :props :c-expr-init-declarator
					 :value assign
					 :lhs decl
					 :rhs init)))
	    (values expr nstate)))
	(.do ((decl (/$declarator state)))
	  (let ((expr (make-c-expression :props :c-expr-init-declarator
					 :lhs decl)))
	    (values expr nstate)))))

(defun-memo /$storage-class-specifier (state)
  (ddprint `(/$storage-class-specifier ,state))
  (.any (//typedef state) (//extern state) (//static state)
	(//_thread_local state) (//auto state) (//register state)))


(defun-memo /$type-specifier (state)
  (ddprint `(/$type-specifier ,state))
  (.do ((specifier (.any (//void state) (//char state) (//short state)
			 (//int state) (//long state) (//float state)
			 (//double state) (//signed state) (//unsigned state)
			 (//_bool state) (//_complex state)
			 (/$atomic-type-specifier state)
			 (/$enum-specifier state)
			 (/$typedef-name state))))
    (let ((expr (make-c-expression :props :c-expr-type-specifier
				   :lhs specifier)))
      (values expr nstate))))

(defun-memo /$struct-or-union-specifier (state)
  (ddprint `(/$struct-or-union-specifier ,state))
  (.any (.do ((op (.any (//struct state) (//union state)))
	      (tag (//identifier nstate))
	      (lbrace (//lbrace nstate))
	      (decl-list (/$struct-declaration-list nstate))
	      (rbeace (//rbrace nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-struct-or-union-specifier
				     :value op
				     :lhs decl-list
				     :rhs tag)))
	    (values expr nstate)))
	(.do ((op (.any (//struct state) (//union state)))
	      (lbrace (//lbrace nstate))
	      (decl-list (/$struct-declaration-list nstate))
	      (rbeace (//rbrace nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-struct-or-union-specifier
				     :value op
				     :lhs decl-list)))
	    (values expr nstate)))
	(.do ((op (.any (//struct state) (//union state)))
	      (tag (//identifier nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-struct-or-union-specifier
				     :value op
				     :rhs tag)))
	    (values expr nstate)))))

(define-left-assoc struct-declaration-list /$struct-declaration //comma)

(defun-memo /$struct-declaration (state)
  (ddprint `(/$struct-declaration ,state))
  (.any (.do ((spec (/$specifier-qualifier-list state))
	      (decl (/$struct-declarator-list nstate)))
	  (let ((expr (make-c-expression :props :c-expr-struct-declaration
					 :lhs spec
					 :rhs decl)))
	    (values expr nstate)))
	(.do ((spec (/$specifier-qualifier-list state)))
	  (let ((expr (make-c-expression :props :c-expr-struct-declaration
					 :lhs spec)))
	    (values expr nstate)))
	(.do ((expr (/$static_assert-declaration state)))
	  (values expr nstate))))

(define-right-assoc specifier-qualifier-list
    (lambda (state) (.any (/$type-specifier state) (/$type-qualifier state))))

(define-left-assoc struct-declarator-list /$struct-declarator //comma)

(defun-memo /$struct-declarator (state)
  (ddprint `(/$struct-declarator ,state))
  (.any (.do ((decl (/$declarator state))
	      (colon (//colon nstate))
	      (expr (/$constant-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-struct-declarator
					 :value colon
					 :lhs decl
					 :rhs expr)))
	    (values expr nstate)))
	(.do ((colon (//colon state))
	      (expr (/$constant-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-struct-declarator
					 :value colon
					 :rhs expr)))
	    (values expr nstate)))
	(.do ((decl (/$declarator state)))
	  (let ((expr (make-c-expression :props :c-expr-struct-declarator
					 :lhs decl)))
	    (values expr nstate)))))

(defun-memo /$enum-specifier (state)
  (ddprint `(/$enum-specifier ,state))
  (.any (.do ((enum (//enum state))
	      (ident (//identifier nstate))
	      (lbrace (//lbrace nstate))
	      (enum-list (/$enumerator-list nstate))
	      (comma (//comma nstate))
	      (rbrrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enum-specifier
					 :value enum
					 :lhs ident
					 :rhs enum-list)))
	    (values expr nstate)))
	(.do ((enum (//enum state))
	      (ident (//identifier nstate))
	      (lbrace (//lbrace nstate))
	      (enum-list (/$enumerator-list nstate))
	      (rbrrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enum-specifier
					 :value enum
					 :lhs ident
					 :rhs enum-list)))
	    (values expr nstate)))
	(.do ((enum (//enum state))
	      (lbrace (//lbrace nstate))
	      (enum-list (/$enumerator-list nstate))
	      (comma (//comma nstate))
	      (rbrrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enum-specifier
					 :value enum
					 :rhs enum-list)))
	    (values expr nstate)))
	(.do ((enum (//enum state))
	      (lbrace (//lbrace nstate))
	      (enum-list (/$enumerator-list nstate))
	      (rbrrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enum-specifier
					 :value enum
					 :rhs enum-list)))
	    (values expr nstate)))
	(.do ((enum (//enum state))
	      (ident (//identifier nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enum-specifier
					 :value enum
					 :lhs ident)))
	    (values expr nstate)))))

(define-left-assoc enumerator-list /$enumerator //comma)

(defun-memo /$enumerator (state)
  (ddprint `(/$enumerator ,state))
  (.any (.do ((const (//enumeration-constant state))
	      (equal (//assign nstate))
	      (value (/$constant-expression nstate)))
	  (let ((expr (make-c-expression :props :c-expr-enumerator
					 :value equal
					 :lhs const
					 :rhs value)))
	    (values expr nstate)))
	(.do ((const (//enumeration-constant state)))
	  (let ((expr (make-c-expression :props :c-expr-enumerator
					 :lhs const)))
	    (values expr nstate)))))

(defun-memo /$atomic-type-specifier (state)
  (.do ((atomic (//_atomic state))
	(lparen (//lparen nstate))
	(type-name (/$type-name nstate))
	(rparen (//rparen nstate)))
    (let ((expr (make-c-expression :props :c-expr-atomic-type-specifier
				   :value atomic
				   :lhs type-name)))
      (values expr nstate))))

(defun-memo /$type-qualifier (state)
  (.do ((qualifier (.any (//const state) (//restrict state)
			 (//volatile state) (//_atomic state))))
    (let ((expr (make-c-expression :props :c-expr-type-qualifier
				   :lhs qualifier)))
      (values expr nstate))))

(defun-memo /$function-specifier (state)
  (.any (//inline state) (//_noreturn state)))

(defun-memo /$alignment-specifier (state)
  (.do ((alignas (//_alignas state))
	(lparen (//lparen nstate))
	(object (.any (/$type-name nstate) (/$constant-expression nstate)))
	(rparen (//rparen nstate)))
    (let ((expr (make-c-expression :props :c-expr-alignment-speciier
				   :value alignas
				   :lhs object)))
      (values expr nstate))))

(defun-memo /$declarator (state)
  (.any (.do ((pointer (/$pointer state))
	      (decl (/$direct-declarator nstate)))
	  (let ((expr (make-c-expression :props :c-expr-declarator
					 :lhs pointer
					 :rhs decl)))
	    (values expr nstate)))
	(.do ((decl (/$direct-declarator state)))
	  (let ((expr (make-c-expression :props :c-expr-declarator
					 :rhs decl)))
	    (values expr nstate)))))


(defun-memo /$%%direct-declarator (state)
  (.any (.do ((lbraket (//lbraket state))
	      (qual-list (/$type-qualifier-list nstate))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lbraket
					 :lhs qual-list
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lbraket
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (qual-list (/$type-qualifier-list nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lbraket
					 :lhs qual-list)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lbraket)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (static (//static nstate))
	      (qual-list (/$type-qualifier-list nstate))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (setf (cpp-token-attr lbraket) :static)
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value :lbaraket
					 :lhs qual-list
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (static (//static nstate))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (setf (cpp-token-attr lbraket) :static)
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value :lbaraket
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (qual-list (/$type-qualifier-list nstate))
	      (static (//static nstate))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (setf (cpp-token-attr lbraket) :static)
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value :lbaraket
					 :lhs qual-list
					 :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (qual-list (/$type-qualifier-list nstate))
	      (star (//mul nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value :lbaraket
					 :lhs qual-list
					 :rhs star)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (star (//mul nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value :lbaraket
					 :rhs star)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (param-list (/$parameter-type-list nstate))
	      (rparen (//rparen nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lparen
					 :lhs param-list)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (id-list (/$identifier-list nstate))
	      (rparen (//rparen nstate)))
	  (let ((expr (make-c-expression :props :c-expr-direct-declarator
					 :value lparen
					 :lhs id-list)))
	    (values expr nstate)))))

(define-left-assoc %direct-declarator /$%%direct-declarator)

(defun-memo /$direct-declarator (state)
  (ddprint `(/$direct-declarator ,state))
  (.do ((lhs
	 (.any (//identifier state)
	       (.do ((lparen (//lparen state))
		     (lhs (/$expression nstate))
		     (rparen (//rparen nstate)))
		 (let ((expr
			 (make-c-expression :props :c-expr-direct-declarator
					    :value lparen
					    :lhs lhs)))
		   (values expr nstate)))))
	(rest (/$%direct-declarator nstate)))
    (let ((expr (make-c-expression :props :c-expr-direct-declarator
				   :lhs lhs
				   :rhs rest)))
      (values expr nstate))))

(defun-memo /$pointer (state)
  (ddprint `(/$pointer ,state))
  (.any (.do ((star (//mul state))
	      (qlist (/$type-qualifier-list nstate))
	      (rhs (/$pointer nstate)))
	  (let ((expr (make-c-expression :props :c-expr-pointer
					 :value star
					 :lhs qlist
					 :rhs rhs)))
	    (values expr nstate)))
	(.do ((star (//mul state))
	      (qlist (/$type-qualifier-list nstate)))
	  (let ((expr (make-c-expression :props :c-expr-pointer
					 :value star
					 :lhs qlist)))
	    (values expr nstate)))
	(.do ((star (//mul state)))
	  (let ((expr (make-c-expression :props :c-expr-pointer
					 :value star)))
	    (values expr nstate)))))

(define-left-assoc type-qualifier-list /$type-qualifier)

(defun-memo /$parameter-type-list (state)
  (ddprint `(/$parameter-type-list ,state))
  (.any (.do ((params (/$parameter-list state))
	      (comma (//comma nstate))
	      (dots (//three-dots nstate)))
	  (let ((expr (make-c-expression :props :c-expr-parameter-type-list
					 :value dots
					 :lhs params)))
	    (values expr nstate)))
	(.do ((params (/$parameter-list state)))
	  (let ((expr (make-c-expression :props :c-expr-parameter-type-list
					 :lhs params)))
	    (values expr nstate)))))

(define-left-assoc parameter-list /$parameter-declaration //comma)

(defun-memo /$parameter-declaration (state)
  (ddprint `(/$parameter-declaration ,state))
  (.any (.do ((spec (/$declaration-specifiers state))
	      (decl (/$declarator nstate)))
	  (let ((expr (make-c-expression :props :c-expr-parameter_declaration
					 :lhs spec
					 :rhs decl)))
	    (values expr nstate)))
	(.do ((spec (/$declaration-specifiers state))
	      (decl (/$abstract-declarator nstate)))
	  (let ((expr (make-c-expression :props :c-expr-parameter_declaration
					 :lhs spec
					 :rhs decl)))
	    (values expr nstate)))
	(.do ((spec (/$declaration-specifiers state)))
	  (let ((expr (make-c-expression :props :c-expr-parameter_declaration
					 :lhs spec)))
	    (values expr nstate)))))

(define-left-assoc identifier-list //identifier //comma)

(defun-memo /$type-name (state)
  (ddprint `(/$type-name ,state))
  (.any (.do ((spec-list (/$specifier-qualifier-list state))
	      (decl (/$abstract-declarator nstate)))
	  (let ((expr (make-c-expression :props :c-expr-type-name
					 :lhs spec-list
					 :rhs decl)))
	    (values expr nstate)))
	(.do ((spec-list (/$specifier-qualifier-list state)))
	  (let ((expr (make-c-expression :props :c-expr-type-name
					 :lhs spec-list)))
	    (values expr nstate)))))

(defun-memo /$abstract-declarator (state)
  (ddprint `(/$abstract-declarator ,state))
  (.any (.do ((pointer (/$pointer state)))
	  (let ((expr (make-c-expression :props :c-expr-abstruct-declarator
					 :lhs pointer)))
	    (values expr nstate)))
	(.do ((pointer (/$pointer state))
	      (declarator (/$direct-abstruct-declarator nstate)))
	  (let ((expr (make-c-expression :props :c-expr-abstruct-declarator
					 :lhs pointer
					 :rhs declarator)))
	    (values expr nstate)))
	(.do ((declarator (/$direct-abstruct-declarator state)))
	  (let ((expr (make-c-expression :props :c-expr-abstruct-declarator
					 :rhs declarator)))
	    (values expr nstate)))))

(defun-memo /$%%direct-abstruct-declarator (state)
  (ddprint `(/$%%direct-abstruct-declarator ,state))
  (.any (.do ((lbraket (//lbraket state))
	      (tqlist (/$type-qualifier-list nstate))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lbraket
				     :lhs tqlist
				     :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (assign (/$assignment-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lbraket
				     :rhs assign)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (tqlist (/$type-qualifier-list nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lbraket
				     :lhs tqlist)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (rbraket (//rbraket nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lbraket)))
	    (values expr nstate)))
	(.do ((lbraket (//lbraket state))
	      (star (//mul nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lbraket
				     :lhs star)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (param (/$parameter-type-list nstate))
	      (rparen (//rparen nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lparen
				     :lhs param)))
	    (values expr nstate)))
	(.do ((lparen (//lparen state))
	      (rparen (//rparen nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lparen)))
	    (values expr nstate)))))

(define-left-assoc %direct-abstruct-declarator /$%%direct-abstruct-declarator)

(defun-memo /$direct-abstruct-declarator (state)
  (ddprint `(/$direct-abstruct-declarator ,state))
  (.any (.do ((lparen (//lparen state))
	      (decl (/$abstract-declarator nstate))
	      (reparen (//rparen nstate))
	      (rest (/$%direct-abstruct-declarator nstate)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :value lparen
				     :lhs decl
				     :rhs rest)))
	    (values expr nstate)))
	(.do ((rest (/$%direct-abstruct-declarator state)))
	  (let ((expr
		  (make-c-expression :props :c-expr-direct-abstruct-declarator
				     :rhs rest)))
	    (values expr nstate)))))

(defun-memo /$typedef-name (state)
  (ddprint `(/$typedef-name ,state))
  (.do ((name (//identifier state)))
    (let ((expr (make-c-expression :props :c-expr-typedef-name
				   :lhs name)))
      (values expr nstate))))

(defun-memo /$initializer (state)
  (ddprint `(/$initializer ,state))
  (.any (.do ((assign (/$assignment-expression state)))
	  (let ((expr (make-c-expression :props :c-expr-initializer
					 :lhs assign)))
	    (values expr nstate)))
	(.do ((lbrace (//lbrace state))
	      (list (/$initializer-list nstate))
	      (comma (//comma nstate))
	      (rbrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-initializer
					 :value lbrace
					 :lhs list)))
	    (values expr nstate)))
	(.do ((lbrace (//lbrace state))
	      (list (/$initializer-list nstate))
	      (rbrace (//rbrace nstate)))
	  (let ((expr (make-c-expression :props :c-expr-initializer
					 :value lbrace
					 :lhs list)))
	    (values expr nstate)))))

(defun-memo /$%inititializer-list (state)
  (ddprint `(/$%inititializer-list ,state))
  (.any (.do ((design (/$designation state))
	      (init (/$initializer nstate)))
	  (let ((expr (make-c-expression :props :c-expr-initializer-list
					 :lhs design
					 :rhs init)))
	    (values expr nstate)))
	(.do ((init (/$initializer state)))
	  (let ((expr (make-c-expression :props :c-expr-initializer-list
					 :rhs init)))
	    (values expr nstate)))))

(define-left-assoc initializer-list /$%inititializer-list //comma)

(defun-memo /$designation (state)
  (ddprint `(/$designation ,state))
  (.do ((list (/$designator-list state))
	(= (//eq nstate)))
    (let ((expr (make-c-expression :props :c-expr-designation
				   :value =
				   :lhs list)))
      (values expr nstate))))

(define-left-assoc designator-list /$designator)

(defun-memo /$designator (state)
  (ddprint `(/$designator ,state))
  (.any (.do ((lbraket (//lbraket state))
	      (const (/$constant-expression nstate))
	      (rbraket (//rbraket nstate)))
	  (let ((expr (make-c-expression :props :c-expr-designator
					 :value lbraket
					 :lhs const)))
	    (values expr nstate)))
	(.do ((dot (//dot state))
	      (ident (//identifier nstate)))
	  (let ((expr (make-c-expression :props :c-expr-designator
					 :value dot
					 :lhs ident)))
	    (values expr nstate)))))

(defun-memo /$static_assert-declaration (state)
  (ddprint `(/$static_assert-declaration ,state))
  (.do ((kwd (//_static_assert state))
	(lparen (//lparen nstate))
	(const (/$constant-expression nstate))
	(string (//string-literal nstate))
	(rparen (//rparen nstate))
	(semicolon (//semicolon nstate)))
    (let ((expr (make-c-expression :props :c-expr-static_assert-declaration
				   :value kwd
				   :lhs const
				   :rhs string)))
      (values expr nstate))))

(defun-memo cpp-eval-expr (expr)
  #+nil
  (print `(cpp-eval-expr ,(if (typep expr 'c-expression)
			      (expr-props expr)
			      (cpp-token-props expr))
			 ,expr))
  #+nil
  (print `(cpp-eval-expr ,expr))
  (etypecase expr
    (cpp-token
     (ecase (cpp-token-props expr)
       (:integer-constant (cpp-token-value expr))))
    (c-expression
     (ecase (expr-props expr)
       (:c-expr-primary-expression
	(let ((op (expr-value expr)))
	  #+nil
	  (print `(:c-expr-primary-expression ,(cpp-token-props op)))
	  (ecase (cpp-token-props op)
	    (:c-punctuator-lparen
	     (cpp-eval-expr (expr-lhs expr))))))
       (:c-expr-unary-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr))))
	  #+nil(print `(cpp-eval-expr unary-expression ,lhs))
	  (ecase (cpp-token-props (expr-value expr))
	    (:c-punctuator-plus (+ lhs))
	    (:c-punctuator-minos (- lhs))
	    (:c-punctuator-bitnot (lognot lhs))
	    (:c-punctuator-lognot (if (zerop lhs) 1 0)))))
       (:c-expr-additive-expression
	(let ((op (expr-value expr))
	      (lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (ecase (cpp-token-props op)
	    (:c-punctuator-plus (+ lhs rhs))
	    (:c-punctuator-minos (- lhs rhs)))))
       (:c-expr-bitand-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (logand lhs rhs)))
       (:c-expr-bitior-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (logxor lhs rhs)))
       (:c-expr-bitxor-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (logior lhs rhs)))
       (:c-expr-conditional-expression
	(let ((test (cpp-eval-expr (expr-value expr))))
	  (if (zerop test)
	      (cpp-eval-expr (expr-rhs expr))
	      (cpp-eval-expr (expr-lhs expr)))))
       (:c-expr-equality-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (ecase (cpp-token-props (expr-value expr))
	    (:c-punctuator-eq (if (= lhs rhs) 1 0))
	    (:c-punctuator-ne (if (/= lhs rhs) 1 0)))))
       (:c-expr-logand-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr))))
	  (if (zerop lhs)
	      0
	      (let ((rhs (cpp-eval-expr (expr-rhs expr))))
		(if (zerop rhs) 0 1)))))
       (:c-expr-logior-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr))))
	  (if (/= lhs 0)
	      1
	      (let ((rhs (cpp-eval-expr (expr-rhs expr))))
		(if (zerop rhs) 0 1)))))
       (:c-expr-multiplicative-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (ecase (cpp-token-props (expr-value expr))
	    (:c-punctuator-mul (* lhs rhs))
	    (:c-punctuator-div (/ lhs rhs))
	    (:c-punctuator-mod (mod lhs rhs)))))
       (:c-expr-relational-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (ecase (cpp-token-props (expr-value expr))
	    (:c-punctuator-gt (if (> lhs rhs) 1 0))
	    (:c-punctuator-ge (if (>= lhs rhs) 1 0))
	    (:c-punctuator-lt (if (< lhs rhs) 1 0))
	    (:c-punctuator-le (if (<= lhs rhs) 1 0)))))
       (:c-expr-shift-expression
	(let ((lhs (cpp-eval-expr (expr-lhs expr)))
	      (rhs (cpp-eval-expr (expr-rhs expr))))
	  (ecase (cpp-token-props (expr-value expr))
	    (:c-punctuator-lsh (ash lhs rhs))
	    (:c-punctuator-rsh (ash lhs (- rhs))))))))))
