(in-package :c-stmt)

(defvar *debug* nil)

(defvar *typedef-hash* (make-hash-table :test #'equal))
(defvar *struct-hash* (make-hash-table :test #'equal))
(defvar *union-hash* (make-hash-table :test #'equal))
(defvar *enumerattor-hash* (make-hash-table :test #'equal))

(defvar *__int8_t-registered* nil)
(defvar *met-typedef* nil)

(defun $/? (state parser)
  (let ((acc (makeq 'c-expression)))
    (multiple-value-bind (expr nstate) (funcall parser state)
	(if (null expr)
	    (values acc state)
	    (values (enq expr acc) nstate)))))

(defun /!many-enq (state parser &optional
				  (acc (makeq 'c-statement))
				  (min 0)
				  (max #.(1- array-dimension-limit))
				  (cnt 0))
  #+nil
  (print `(/!many-enq :ENTRY ,acc))
  (multiple-value-bind (result nstate) (funcall parser state)
    #+nil
    (print `(/!many-enq ,parser ,cnt ,result ,nstate))
    (if (or (>= cnt max) (null result))
	(if (< cnt min)
	    (values nil nil)
	    (progn
	      #+nil
	      (print `(/!many-enq ,acc))
	      (values acc state)))
	(/!many-enq nstate parser (enq result acc) min max (1+ cnt)))))

(defun dprint (state name)
  (when *debug*
    (let* ((vec (is-i state))
	   (ofs (is-o state))
	   (len (length vec)))
      (when (< ofs len)
	(print `(,name :ENTER ,(cpp-token-value (aref vec ofs)) ,ofs))))))

(defun dfmt (sym obj)
  (format t "~%(~S ACCEPTS ~S)" sym obj))

;;;
;;; declaration
;;;
(defstruct (c-declaration (:include token)
			  (:conc-name decl-) (:constructor mkdecl))
  (body nil :type t))

(defun return-decl (prop value body state &optional (caller nil))
  (declare (ignorable caller))
  (unless state
    (break))
  #+nil
  (if caller
      (print `(return-decl >>> ,prop ,value ,body ,state ,caller))
      (print `(return-decl >>> ,prop ,value ,body ,state)))
  (values (mkdecl :props prop :value value :body body) state))

(defun return-decl-enq (prop value body state acc)
  (let ((decl (mkdecl :props prop :value value :body body)))
    (unless state
      (break))
    (when acc
      (enq decl acc))
    #+nil
    (print `(return-decl-enq >>> ,prop ,value ,body ,state ,acc))
    (values decl state)))

(defun //identifier* (state)
  (.do ((token (//identifier state)))
    (let* ((id (cpp-token-value token))
	   (typedef-found (nth-value 1 (gethash id *typedef-hash*)))
	   (enum-found (nth-value 1 (gethash id *enumerattor-hash*))))
      #+nil
      (print `(:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ //identifier*
	       :id ,id :typedef ,typedef-found :enum ,enum-found))
       (if (and (not typedef-found) (not enum-found))
	   (values token nstate)
	   (values nil nil)))))

(defun //tag-name (state)
  (.do ((token (token-input state)))
    (multiple-value-bind (_ found)
	(gethash (cpp-token-value token) *typedef-hash*)
      (declare (ignorable _))
      (when found
	(values token nstate)
	(values nil nil)))))

(defun />declaration (state)
  (dprint state '/>declaration)
  #+nil
  (when *force-print-typedef-name*
    (print `(:++++++++++++++++ :state==> ,state)))
  #+nil
  (print `(:>>>>>>>> />declaration :<<<< :state==> ,state))
  (.any (.do ((decl-spec (/>declaration-specifiers state))
	      (init-decl (/>init-declarator-list nstate))
	      (semi (//semicolon nstate)))
	  (return-decl :c-declaration decl-spec init-decl nstate))
	(.do ((decl-spec (/>declaration-specifiers state))
	      (semi (//semicolon nstate)))
	  (return-decl :c-declaration decl-spec nil nstate))
	(.do ((decl-spec (/>static_assert-declaration state)))
	  (return-decl :c-declaration decl-spec nil nstate))))

(defun />declaration-specifiers (state)
  (dprint state  '/>declaration-specifier)
  (.do ((list (/!many-enq state #'/>declaration-specifier)))
    (return-decl :c-decl-declaration-specifiers list nil nstate)))

(defun />declaration-specifier (state)
  (dprint state  '/>declaration-specifier)
  (.do ((spec (.any (/>storage-class-specifier state)
		    (/>type-specifier state)
		    (/>type-qualifier state)
		    (/>function-specifier state)
		    (/>alignment-specifier state))))
    #+nil
    (print `(:$$$$$$$$ />declaration-specifier :@ ,spec))
    (values spec nstate)))

(defun />init-declarator-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>init-declarator-list)
  (.any (.do ((decl (/>init-declarator state acc))
	      (rest (/>init-declarator-list-rest nstate acc)))
	  (return-decl :c-decl-init-declarator-list acc nil nstate))
	(.do ((decl (/>init-declarator state acc)))
	  (return-decl :c-decl-init-declarator-list acc nil nstate))))

(defun />init-declarator-list-rest (state acc)
  (dprint state  '/>init-declarator-list-rest)
  (/many* state #'/>init-declarator-list-rest-elt acc))

(defun />init-declarator-list-rest-elt (state acc)
  (dprint state  '/>init-declarator-list-rest-elt)
  (.do ((comma (//comma state))
	(init-decl (/>init-declarator nstate acc)))
    (values init-decl nstate)))

(defun />init-declarator (state acc)
  (dprint state  '/>init-declarator)
  (.any (.do ((decl (/>declarator state))
	      (equal (//assign nstate))
	      (init (/>initializer nstate)))
	  (return-decl-enq :c-decl-init-declarator decl init nstate acc))
	(.do ((decl (/>declarator state)))
	  (return-decl-enq :c-decl-init-declarator decl nil nstate acc))))

(defun />storage-class-specifier (state)
  (dprint state  '/>storage-class-specifier)
  (.do ((token (.any (//typedef state) (//extern state) (//static state)
		     (//auto state) (//register state)
		     (//_Thread_local state))))
    #+nil
    (print `(/>storage-class-specifier "ACCEPTED TOKEN"
				       ,(cpp-token-value token)))
    (return-decl :c-decl-storage-class-specifier token nil nstate)))

(defun />type-specifier (state)
  (dprint state  '/>type-specifier)
  (.do ((spec (.any (//void state) (//char state) (//short state)
		    (//int state)  (//long state)
		    (//float state) (//double state)
		    (//signed state) (//unsigned state)
		    (//_Bool state) (//_Complex state)
		    (/>atomic-type-specifier state)
		    (/>struct/union-specifier state)
		    (/>enum-specifier state)
		    (/>typedef-name state))))
    #+nil
    (print `(:>>>>>>>> />type-specifier :@ :accept==> ,spec))
    (return-decl :c-decl-type-specifier spec nil nstate '/>type-specifier)))

(defun />struct/union-specifier (state)
  (dprint state  '/>struct/union-specifier)
  #+nil
  (print `(:>>>>>>>> />struct/union-specifier :<<<< :state==> ,state))
  (flet ((fn-ret (type ident decl state)
	   #+nil
	   (print `(/>struct/union-specifier :exit
		    :type=> ,type ,(cpp-token-props type)))
	   (let ((props (if (eq (cpp-token-props type) :c-keyword-struct)
			    :c-decl-struct-specifier
			    :c-decl-union-specifier)))
	     (return-decl props ident decl state))))
    (.any (.do ((type (multiple-value-bind (type nstate)
			  (//struct/union state)
			#+nil
			(print `(:>>>>>>>> />struct/union-specifier :1b1
					   :type==> ,type))
			(values type nstate)))
		(ident (multiple-value-bind (ident nstate)
			   (//identifier* nstate)
			 #+nil
			 (print `(:>>>>>>>> />struct/union-specifier :1b2
					    :ident==> ,ident))
			 (values ident nstate)))
		(lbrace (multiple-value-bind (lbrace nstate) (//lbrace nstate)
			  #+nil
			  (print `(:>>>>>>>> />struct/union-specifier :1b3
					     :lbrace==> ,lbrace))
			  (values lbrace nstate)))
		(decl-list (multiple-value-bind (decl-list nstate)
			       (/>struct-declaration-list nstate)
			     #+nil
			     (print `(:>>>>>>>> />struct-declaration-list :1b4
						:decl-list==> ,decl-list))
			     (values decl-list nstate)))
		(rbrace (multiple-value-bind (rbrace nstate)
			    (//rbrace nstate)
			  #+nil
			  (print `(:>>>>>>>> />struct-declaration-list :1b5
					     :rbrace==> ,rbrace))
			  (values rbrace nstate))))
	    #+nil
	    (print `(:>>>>>>>> />struct/union-specifier :@
		     :type==> ,type :ident==>,ident :decl==> ,decl-list))
	    (fn-ret type ident decl-list nstate))
	  (.do ((type (//struct/union state))
		(lbrace (//lbrace nstate))
		(decl-list (/>struct-declaration-list nstate))
		(rbrace (//rbrace nstate)))
	    #+nil
	    (print `(:>>>>>>>> />struct/union-specifier :@@
		     :type==> ,type :ident==> :none :decl==> .decl-list))
	    (fn-ret type nil decl-list nstate))
	  (.do ((type (//struct/union state))
		(ident (//identifier* nstate)))
	    #+nil
	    (print `(:>>>>>>>> />struct/union-specifier :@@@
		     :type==> ,type :idnt==> ,ident))
	    (fn-ret type ident nil nstate)))))

(defun //struct/union (state)
  (dprint state  '//struct/union)
  (.any (//struct state) (//union state)))

(defun />struct-declaration-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>struct-declaration-list)
  #+nil
  (setf cpp-directive::*do-print-/many** #'/>struct-declaration)
  (multiple-value-bind (result nstate)
      (/many* state #'/>struct-declaration acc)
    #+nil
    (setf cpp-directive::*do-print-/many** nil)
    (values result nstate)))

(defun />struct-declaration (state acc)
  (dprint state  '/>struct-declaration)
  (flet ((fn-ret (spec decl nstate)
	   (let ((decl (mkdecl :props :c-decl-struct-declaration
			       :value spec :body decl)))
	     (when acc
	       (enq decl acc)
	       #+nil
	       (print `(:???????? />struct-declaration :>>>>
				  :acc ,acc :nstate ,state)))
	     (values decl nstate))))
    #+nil
    (print `(:???????? />struct-declaration :<<<< :state ,state
						  #+nil :acc #+nil ,acc))
    (.any (.do ((spec-list (progn
			     #+nil
			     (print `(:???????? />struct-declaration
				      :spec-list
				      :state ,state))
			     (multiple-value-bind (spec-list nstate)
				 (/>specifier-qualifier-list state)
			       #+nil
			       (print `(/>struct-declaration
					:spec==> ,spec-list
					:nstae==> ,nstate))
			       (values spec-list nstate))))
		(decl-list (multiple-value-bind (decl-list nstate)
			       (/>struct-declarator-list nstate)
			     #+nil
			     (print `(/>struct-declaration :decl=> ,decl-list))
			     (values decl-list nstate)))
		(semicolon (multiple-value-bind (semi nstate)
			       (//semicolon nstate)
			     #+nil
			     (print `(/>struct-declaration semicolon))
			     (values semi nstate))))
	    #+nil
	    (print `(:???????? />struct-declaration
		     :spec ,spec-list :decl ,decl-list))
	    (fn-ret spec-list decl-list nstate))
	  (.do ((static-assert (/>static_assert-declaration state)))
	    (fn-ret static-assert nil nstate)))))

(defun />specifier-qualifier-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>specifier-qualifier-list)
  #+nil
  (print :********************************)
  (.do ((spec/qual (multiple-value-bind (spec/qual nstate)
		       (/many* state #'/>specifier-qualifier acc)
		     #+nil
		     (print `(:???????? />specifier-qualifier-list :==
					:spec ,spec/qual))
		     (values spec/qual nstate))))
    (return-decl :c-decl-specifier-qualifier-list spec/qual nil nstate)))

(defun />specifier-qualifier (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>specifier-qualifier)
  (when (< (is-o state) (length (is-i state)))
    #+nil
    (print `(:???????? />specifier-qualifier :@
		       :input ,(aref (is-i state) (is-o state)))))
  (.do ((elt (.any (/>type-specifier state) (/>type-qualifier state)
		   (/>alignment-specifier state))))
    #+nil
    (print `(:???????? />specifier-qualifier :@@ :elt ,elt))
    (values (enq elt acc) nstate)))

(defun />struct-declarator-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>struct-declarator-list)
  (.do ((list (.any (.do ((first (/>struct-declarator state acc))
			  (rest (/>struct-declarator-rest nstate acc)))
		      (values acc nstate))
		    (.do ((first (/>struct-declarator state acc)))
		      (values acc nstate)))))
    (return-decl :c-decl-struct-declarator-list list nil nstate)))

(defun />struct-declarator-rest (state acc)
  (dprint state  '/>struct-declarator-rest)
  (/many* state #'/>struct-declarator-rest-elt acc))

(defun />struct-declarator-rest-elt
    (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>struct-declarator-rest-elt)
  (.do ((commna (//comma state))
	(declarator (/>struct-declarator nstate acc)))
    #+nil
    (print `(/>struct-declarator ,acc))
    (values declarator nstate)))

(defun />struct-declarator (state acc)
  (dprint state  '/>struct-declarator)
  (.any (.do ((decl (/>declarator state))
	      (colon (//colon nstate))
	      (expr (/$constant-expression nstate)))
	  (return-decl-enq :c-decl-struct-declarator decl expr nstate acc))
	(.do ((colon (//colon state))
	      (expr (/$constant-expression nstate)))
	  (return-decl-enq :c-decl-struct-declarator nil expr nstate acc))
	(.do ((decl (/>declarator state)))
	  #+nil
	  (print `(:???????? />struct-declarator :@ :decl==> ,decl))
	  (return-decl-enq :c-decl-struct-declarator decl nil nstate acc))))

(defun />enum-specifier (state)
  (dprint state  '/>enum-specifier)
  (flet ((fn-ret (ident enum-list nstate)
	   #+nil
	   (when enum-list
	     (loop :for elt :across enum-list
		   :when (eq (decl-props elt) :c-decl-enumerator)
		     :do (let* ((token (decl-value elt))
				(name (cpp-token-value token))
				(found
				  (nth-value 1 (gethash name
							*enumerattor-hash*))))
			   (when found
			     (error "redefinition of enumerator '~S'." name))
			   (setf (gethash name *enumerattor-hash*) token))))
	   (return-decl :c-decl-enum-specifier ident enum-list nstate)))
    (.any (.do ((enum (//enum state))
		(ident (//identifier* nstate))
		(lbrace (//lbrace nstate))
		(enum-list (/>enumerator-list nstate))
		(comma (//comma nstate))
		(rbrace (//rbrace nstate)))
	    (fn-ret ident enum-list nstate))
	  (.do ((enum (//enum state))
		(lbrace (//lbrace nstate))
		(enum-list (/>enumerator-list nstate))
		(comma (//comma nstate))
		(rbrace (//rbrace nstate)))
	    (fn-ret nil enum-list nstate))
	  (.do ((enum (//enum state))
		(ident (//identifier* nstate))
		(lbrace (//lbrace nstate))
		(enum-list (/>enumerator-list nstate))
		(rbrace (//rbrace nstate)))
	    (fn-ret ident enum-list nstate))
	  (.do ((enum (//enum state))
		(lbrace (//lbrace nstate))
		(enum-list (/>enumerator-list nstate))
		(rbrace (//rbrace nstate)))
	    (fn-ret nil enum-list nstate))
	  (.do ((enum (//enum state))
		(ident (//identifier* nstate)))
	    (fn-ret ident nil nstate)))))

(defun />enumerator-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>enumerator-list)
  (.any (.do ((first (/>enumerator state acc))
	      (rest (/>enumerator-rest nstate acc)))
	  (values acc nstate))
	(.do ((first (/>enumerator state acc)))
	  (values acc nstate))))

(defun />enumerator-rest (state acc)
  (dprint state  '/>enumerator-rest)
  (/many* state #'/>enumerator-rest-elt acc))

(defun />enumerator-rest-elt (state acc)
  (dprint state  '/>enumerator-rest-elt)
  (.do ((comma (//comma state))
	(enum (/>enumerator nstate acc)))
    (values acc nstate)))

(defun />enumerator (state acc)
  (dprint state  '/>enumerator)
  (flet ((fn-ret (elt val nstate)
	   #+nil
	   (print `(:>>>>>>>> />enumerator :elt==> ,elt))
	   (return-decl-enq :c-decl-enum-specifier elt val nstate acc)))
    (.any (.do ((elt (//enumeration-constant state))
		(eq (//assign nstate))
		(const (/$constant-expression nstate)))
	    (fn-ret elt const nstate))
	  (.do ((elt (//enumeration-constant state)))
	    (fn-ret elt nil nstate)))))

(defun />atomic-type-specifier (state)
  (dprint state  '/>atomic-type-specifier)
  (.do ((atomic (//_atomic state))
	(lparen (//lparen nstate))
	(type-name (/>type-name nstate))
	(rparen (//rparen nstate)))
    (return-decl :c-decl-atomic-type-specifier type-name nil nstate)))

(defun />type-qualifier (state)
  (dprint state  '/>type-qualifier)
  (.do ((qual (.any (//const state) (//restrict state) (//volatile state)
		    (//_atomic state))))
    #+nil
    (dfmt '/>type-qualifier qual)
    (return-decl :c-decl-type-qualifier qual nil nstate)))

(defun />function-specifier (state)
  (dprint state  '/>function-specifier)
  (.do ((spec (.any (//__inline state) (//inline state) (//_Noreturn state))))
    (return-decl :c-decl-func-specifier spec nil nstate)))

(defun />alignment-specifier (state)
  (dprint state  '/>alignment-specifier)
  (.any (.do ((align (//_Alignas state))
	      (lparen (//lparen nstate))
	      (type-name (/>type-name nstate))
	      (rparen (//rparen nstate)))
	  (return-decl :c-decl-alignment-specifier :typename type-name nstate))
	(.do ((align (//_Alignas state))
	      (lparen (//lparen nstate))
	      (const (/$constant-expression nstate))
	      (rparen (//rparen nstate)))
	  (return-decl :c-decl-alignment-specifier :constant const nstate))))

(defun />declarator (state)
  (dprint state  '/>declarator)
  (.any (.do ((ptr (/>pointer state))
	      (decl (/>direct-declarator nstate)))
	  (return-decl :c-decl-declarator ptr decl nstate))
	(.do ((decl (/>direct-declarator state)))
	  #+nil
	  (print `(:???????? />declarator :@ :decl ,decl))
	  (return-decl :c-decl-declarator nil decl nstate))))

(defun />direct-declarator (state)
  (dprint state  '/>direct-declarator)
  (.any (.do ((ident (//identifier* state))
	      (suffix (/>suffixes nstate)))
	  (return-decl :c-decl-direct-declarator ident suffix nstate))
	(.do ((ident (//identifier* state)))
	  (return-decl :c-decl-direct-declarator ident nil nstate))
	(.do ((lparen (//lparen state))
	      (decl (/>declarator nstate))
	      (rparen (//rparen nstate))
	      (suffix (/>suffixes nstate)))
	  (return-decl :c-decl-direct-declarator decl suffix nstate))
	(.do ((lparen (//lparen state))
	      (decl (/>declarator nstate))
	      (rparen (//rparen nstate)))
	  (return-decl :c-decl-direct-declarator decl nil nstate))))

(defun />suffixes (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>suffixes)
  (/many* state #'/>suffix acc))

(defun />suffix (state acc)
  (dprint state  '/>suffix)
  (.any (/>array-suffix state acc)
	(/>fun-arg state acc) (/>fun-proto state acc)))

(defstruct (c-array-index (:include c-declaration)
			  (:conc-name cai-) (:constructor mkcai))
  (index nil :type (or c-expression cpp-token null)))

(defun />array-suffix (state acc)
  (dprint state  '/>array-suffix)
  (.any (.do ((lbraket (//lbraket state))
	      (index (/>array-index nstate acc))
	      (rbraket (//rbraket nstate)))
	  (values index nstate))
	(.do ((lbraket (//lbraket state))
	      (rbraket (//rbraket nstate)))
	  (values (enq (mkcai :props :c-decl-array-index
			      :value nil
			      :body nil
			      :index nil)
		       acc)
		  nstate))))

(defun />array-index (state acc)
  (dprint state   '/>array-index)
  (flet ((fn-ret (value body index nstate)
	   (let ((decl (mkcai :value value :body body :index index)))
	     (when acc
	       (enq decl acc))
	     (values decl nstate))))
    (.any (.do ((static (//static state))
		(qual (/>type-qualifier-list nstate))
		(assign (/$assignment-expression nstate)))
	    (fn-ret static qual assign nstate))
	  (.do ((static (//static state))
		(assign (/$assignment-expression nstate)))
	    (fn-ret static nil assign nstate))
	  (.do ((qual (/>type-qualifier-list state))
		(static (//static nstate))
		(assign (/$assignment-expression nstate)))
	    (fn-ret qual static assign nstate))
	  (.do ((qual (/>type-qualifier-list state))
		(star (//mul nstate)))
	    (fn-ret qual nil star nstate))	
	  (.do ((star (//mul state)))
	    (fn-ret nil nil star nstate))
	  (.do ((assign (/$assignment-expression state)))
	    (fn-ret nil nil assign nstate))
	  (.do ((qual (/>type-qualifier-list state)))
	    (fn-ret qual nil nil nstate)))))

(defun />fun-proto (state acc)
  (dprint state  '/>fun-proto)
  (.any (.do ((lparen (//lparen state))
	      (params (/>parameter-type-list nstate))
	      (rparen (//rparen nstate)))
	  (return-decl-enq :c-function-arguments params nil nstate acc))
	(.do ((lparen (//lparen state))
	      (rparen (//rparen nstate)))
	  (return-decl-enq :c-function-arguments nil nil nstate acc))))

(defun />fun-arg (state acc)
  (dprint state  '/>fun-arg)
  (.any (.do ((lparen (//lparen state))
	      (args (/>identifier-list nstate))
	      (rparen (//rparen nstate)))
	  (return-decl-enq :c-function-arguments args nil nstate acc))
	(.do ((lparen (//lparen state))
	      (rparen (//rparen nstate)))
	  (return-decl-enq :c-function-arguments nil nil nstate acc))))

(defun />pointer (state)
  (dprint state  '/>pointer)
  (.any (.do ((star (//mul state))
	      (qual (/>type-qualifier-list nstate))
	      (ptr (/>pointer nstate)))
	  (return-decl :c-decl-pointer qual ptr nstate))
	(.do ((star (//mul state))
	      (ptr (/>pointer nstate)))
	  (return-decl :c-decl-pointer nil ptr nstate))
	(.do ((star (//mul state))
	      (qual (/>type-qualifier-list nstate)))
	  (return-decl :c-decl-pointer qual nil nstate))
	(.do ((star (//mul state)))
	  (return-decl :c-decl-pointer nil nil nstate))))

(defun />type-qualifier-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>type-qualifier-list)
  (.do ((list (/!many-enq state #'/>type-qualifier acc)))
    (return-decl :c-decl-type-qualifier-list list nil nstate)))

(defun />parameter-type-list (state)
  (dprint state  '/>parameter-type-list)
  (.any (.do ((list (/>parameter-list state))
	      (comma (//comma nstate))
	      ($... (//three-dots nstate)))
	  (return-decl :c-parameter-type-list list $... nstate))
	(.do ((list (/>parameter-list state)))
	  (return-decl :c-parameter-type-list list nil nstate))))

(defun />parameter-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>parameter-list)
  (.any (.do ((first (/>parameter-declaration state acc))
	      (rest  (/>parameter-list-rest nstate acc)))
	  (return-decl :c-decl-parameter-list acc nil nstate))
	(.do ((first (/>parameter-declaration state acc)))
	  (return-decl :c-decl-parameter-list acc nil nstate))))

(defun />parameter-list-rest (state acc)
  (dprint state  '/>parameter-list-rest)
  (/many* state #'/>parameter-list-rest-ent acc))

(defun />parameter-list-rest-ent (state acc)
  (.do ((comma (//comma state))
	(param (/>parameter-declaration nstate acc)))
    #+nil
    (print `(/>parameter-list-rest-ent
	     ,param ,(aref (is-i nstate) (is-o nstate))))
    (let* ((val (decl-value param))
	   (vec (decl-value val)))
      (if (= (length vec) 0)
	  (values nil nil)
	  (values param nstate)))))

(defun />parameter-declaration (state acc)
  (dprint state  '/>parameter-declaration)
  (.any (.do ((spec (/>declaration-specifiers state))
	      (decl (/>declarator nstate)))
	  (return-decl-enq :c-decl-parameter-declaration spec decl nstate acc))
	(.do ((spec (/>declaration-specifiers state))
	      (decl (/>abstract-declarator nstate)))
	  (return-decl-enq :c-decl-parameter-declaration spec decl nstate acc))
	(.do ((spec (/>declaration-specifiers state)))
	  (return-decl-enq :c-decl-parameter-declaration spec nil nstate acc))))

(defun />identifier-list (state &optional (acc (makeq 'c-declaration)))
  (.any (.do ((ident (//identifier* state))
	      (rest (progn
		      (enq ident acc)
		      (/>identifier-list-rest nstate acc))))
	  (return-decl :c-identifier-list acc nil nstate))
	(.do ((ident (//identifier* state)))
	  (enq ident acc)
	  (return-decl :c-identifier-list acc nil nstate))))

(defun />identifier-list-rest (state acc)
  (dprint state  '/>identifier-list-rest)
  (/many* state #'/>identifier-list-rest-elt acc))

(defun />identifier-list-rest-elt (state acc)
  (dprint state  '/>identifier-list-rest-elt)
  (.do ((comma (//comma state))
	(ident (//identifier* nstate)))
    (values (enq ident acc) nstate)))

(defun />type-name (state)
  (dprint state  '/>type-name)
  (.any (.do ((list (/>specifier-qualifier-list state))
	      (decl (/>abstract-declarator nstate)))
	  (return-decl :c-decl-type-name list decl nstate))
	(.do ((list (/>specifier-qualifier-list state)))
	  (return-decl :c-decl-type-name list nil nstate))))

(defun />abstract-declarator (state)
  (dprint state  '/>abstract-declarator)
  (.any (.do ((ptr  (/>pointer state))
	      (decl (/>direct-abstract-declarator nstate)))
	  (return-decl :c-decl-abstract-declarator ptr decl nstate))
	(.do ((decl (/>direct-abstract-declarator state)))
	  (return-decl :c-decl-abstract-declarator nil decl nstate))
	(.do ((ptr  (/>pointer state)))
	  (return-decl :c-decl-abstract-declarator ptr nil nstate))))

(defun />direct-abstract-declarator (state)
  (.any (.do ((lparen (//lparen state))
	      (decl (/>abstract-declarator nstate))
	      (rparen (//rparen nstate))
	      (suffix (/>direct-abstract-declarator-suffixes nstate)))
	  (return-decl :c-decl-direct-abstract-declarator decl suffix nstate))
	(.do ((lparen (//lparen state))
	      (decl (/>abstract-declarator nstate))
	      (rparen (//rparen nstate)))
	  (return-decl :c-decl-direct-abstract-declarator decl nil nstate))))

(defun />direct-abstract-declarator-suffixes
    (state &optional (acc (makeq 'c-declaration)))
    (dprint state  '/>direct-abstract-declarator-suffixes)
  (.do ((list (/many* state #'/>direct-abstract-declarator-suffixes-elt acc)))
    (return-decl :c-decl-direct-abstract-declarator-suffixes list nil nstate)))

(defun />direct-abstract-declarator-suffixes-elt (state acc)
  (dprint state  '/>direct-abstract-declarator-suffixes-elt)
  (.any (/>direct-abstract-array-suffix state acc)
	(/>fun-proto state acc)))

(defun />direct-abstract-array-suffix (state acc)
  (dprint state  '/>direct-abstract-array-suffix)
  (.any (.do ((lbraketn (//lbraket state))
	      (index (/>direct-abstract-array-index nstate acc))
	      (rbraket (//rbraket nstate)))
	  (values index nstate))
	(.do ((lbraketn (//lbraket state))
	      (rbraket (//rbraket nstate)))
	  (values (enq (mkcai :props :c-decl-array-index
			      :value nil
			      :body nil
			      :index nil)
		       acc)
		  nstate))))

(defun />direct-abstract-array-index (state acc)
  (dprint state  '/>direct-abstract-array-index)
  (flet ((fn-ret (value body index nstate)
	   (values (enq (mkcai :props :c-decl-array-index
			       :value value
			       :body body
			       :index index)
			acc)
		   nstate)))
    (.any (.do ((static (//static state))
		(qual (/>type-qualifier-list nstate))
		(assign (/$assignment-expression nstate)))
	    (fn-ret static qual assign nstate))
	  (.do ((static (//static state))
		(assign (/$assignment-expression nstate)))
	    (fn-ret static nil assign nstate))
	  (.do ((qual (/>type-qualifier-list state))
		(static (//static nstate))
		(assign (/$assignment-expression nstate)))
	    (fn-ret qual static assign nstate))
	  (.do ((qual (/>type-qualifier-list state))
		(star (//mul nstate)))
	    (fn-ret qual nil star nstate))
	  (.do ((star (//mul state)))
	    (fn-ret nil nil star nstate))
	  (.do ((assign (/$assignment-expression state)))
	    (fn-ret nil nil assign nstate))
	  (.do ((qual (/>type-qualifier-list state)))
	    (fn-ret qual nil nil nstate)))))

(defvar *force-print-typedef-name* nil)

(defun />typedef-name (state)
  (dprint state  '/>typedef-name)
  (when (< (is-o state) (length (is-i state))))
  (.do ((token (token-input state)))
    (let ((val (cpp-token-value token)))
      (multiple-value-bind (_ found) (gethash val *typedef-hash*)
	(declare (ignorable _))
	#+nil
	(when *force-print-typedef-name*
	  (print `(:>>>>>>>> />typedef-name :@ 0
		   :val==> ,val :entry==> ,_ :found==> ,found)))
	(if found
	    (progn
	      (setf (cpp-token-props token) :typedef-name)
	      (values token nstate))
	    (values nil nil))))))

(defun />initializer (state)
  (dprint state  '/>initializer)
  (.do ((init (.any (/$assignment-expression state)
		    (.do ((lbrace (//lbrace state))
			  (init (/>initializer-list nstate))
			  (comma (//comma nstate))
			  (rbrace (//rbrace nstate)))
		      (values init nstate))
		    (.do ((lbrace (//lbrace state))
			  (init (/>initializer-list nstate))
			  (rbrace (//rbrace nstate)))
		      (values init nstate)))))
    (return-decl :c-decl-initializer init nil nstate)))

(defun />initializer-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>initializer-list)
  (.any (.do ((elt (/>initializer-list-elt state acc))
	      (rest (/>initializer-list-rest nstate acc)))
	  (return-decl :c-decl-initializer-list acc nil nstate))
	(.do ((elt (/>initializer-list-elt state acc)))
	  (return-decl :c-decl-initializer-list acc nil nstate))))

(defun />initializer-list-elt (state acc)
  (dprint state  '/>initializer-list)
  (flet ((fn-ret (design init acc state)
	   (values (enq (mkdecl :props :c-decl-initializer-list-element
				:value design
				:body init)
			acc)
		   state)))
    (.any (.do ((design (/>designation state))
		(init (/>initializer nstate)))
	    (fn-ret design init acc nstate))
	  (.do ((init (/>initializer state)))
	    (fn-ret nil init acc nstate)))))

(defun />initializer-list-rest (state acc)
  (dprint state  '/>initializer-list-rest)
  (/many state #'/>initializer-list-rest-elt acc))

(defun />initializer-list-rest-elt (state acc)
  (dprint state  '/>initializer-list-rest-elt)
  (.do ((comma (//comma state))
	(elt (/>initializer-list-elt nstate acc)))
    (values elt nstate)))

(defun />designation (state)
  (dprint state  '/>designation)
  (.do ((list (/>designator-list state))
	(assign (//assign nstate)))
    (return-decl :c-decl-designation list nil nstate)))

(defun />designator-list (state &optional (acc (makeq 'c-declaration)))
  (dprint state  '/>designator-list)
  (.do ((list (/many* state #'/>designator acc)))
    (return-decl :c-decl-designator-list acc nil nstate)))

(defun />designator (state acc)
  (dprint state '/>designator)
  #+nil
  (print `(/>designator ,acc ,(type-of acc)))
  (flet ((fn-ret (sym val acc nstate)
	   (let ((decl (mkdecl :props :c-decl-designator :value sym :body val)))
	     (enq decl acc)
	     (values decl nstate))))
    (.any (.do ((lbraket (//lbraket state))
		(expr (/$constant-expression nstate))
		(rbraket (//rbraket nstate)))
	    (fn-ret lbraket expr acc nstate))
	  (.do ((dot (//dot state))
		(ident (//identifier* nstate)))
	    (fn-ret dot ident acc nstate)))))

(defun />static_assert-declaration (state)
  (dprint state  '/>static_assert-declaration)
  (.do ((sa (//_static_assert state))
	(lparen (//lparen nstate))
	(expr (/$constant-expression nstate))
	(comma (//comma nstate))
	(str (//string-literal nstate))
	(rpalen (//rparen nstate)))
    (return-decl :c-decl-static_assert-declaration expr str nstate)))
			
;;;
;;; statement
;;;
(defstruct (c-statement (:include c-declaration)
			(:conc-name stmt-) (:constructor mkstmt)))

(defun return-stmt (props value body state)
  (values (mkstmt :props props :value value :body body) state))

(defun return-stmt-enq (props value body state acc)
  (let ((stmt (mkstmt :props props :value value :body body)))
    (when acc
      (enq stmt acc))
    (values stmt state)))

(defun /!statement (state &optional (acc (makeq 'token)))
  (dprint state  '/!statement)
  (.do ((stmt (.any (/!labeled-statement state)
		    (/!compound-statement state)
		    (/!expression-statement state)
		    (/!selection-statement state)
		    (/!iteration-statement state)
		    (/!jump-statement state))))
    (return-stmt-enq :c-stmt-statement stmt nil nstate acc)))

(defun /!labeled-statement (state)
  (dprint state  '/!labeled-statement)
  (.any (.do ((ident (//identifier* state))
	      (colon (//colon nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-labeled-statement ident stmt nstate))
	(.do ((case! (//case state))
	      (expr (/$constant-expression nstate))
	      (colon (//colon nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-labeled-statement-case expr stmt nstate))
	(.do ((dflt (//default state))
	      (colon (//colon nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-labeled-statement-default nil stmt nstate))))

(defun /!compound-statement (state)
  (dprint state  '/!compound-statement)
  (.any (.do ((lbrace (//lbrace state))
	      (list (/!block-item-list nstate))
	      (rbrace (//rbrace nstate)))
	  (return-stmt :c-stmt-compound-statement list nil nstate))
	(.do ((lbrace (//lbrace state))
	      (rbrace (//rbrace nstate)))
	  (return-stmt :c-stmt-compound-statement nil nil nstate))))

(defun /!block-item-list (state &optional (acc (makeq 'c-statement)))
  (dprint state  '/!block-item-list)
  (/many* state #'/!block-item acc))

(defun /!block-item (state acc)
  (dprint state  '/!block-item)
  (.do ((item (.any (/>declaration state) (/!statement state))))
    (return-stmt-enq :c-stmt-block-item item nil nstate acc)))

(defun /!expression-statement (state)
  (dprint state  '/!expression-statement)
  (.any (.do ((expr (/$expression state))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-expression-statement expr nil nstate))
	(.do ((semi (//semicolon state)))
	  (return-stmt :c-stmt-expression-statement nil nil nstate))))

(defun /!selection-statement (state)
  (dprint state  '/!selection-statement)
  (.any (.do (($if (//if state))
	      (lparen (//lparen nstate))
	      (test (/$expression nstate))
	      (rparen (//rparen nstate))
	      (then (/!statement nstate))
	      ($else (//else nstate))
	      (else (/!statement nstate)))
	  (return-stmt :c-stmt-selection-statement-if
		       test (cons then else) nstate))
	(.do (($if (//if state))
	      (lparen (//lparen nstate))
	      (test (/$expression nstate))
	      (rparen (//rparen nstate))
	      (then (/!statement nstate)))
	  (return-stmt :c-stmt-selection-statement-if
		       test (cons then nil) nstate))
	(.do (($switch (//switch state))
	      (lparen (//lparen nstate))
	      (test (/$expression nstate))
	      (rparen (//rparen nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-selection-statement-switch stmt nil nstate))))

(defun /!iteration-statement (state)
  (dprint state  '/!iteration-statement)
  (.any (.do (($while (//while state))
	      (lparen (//lparen nstate))
	      (test (/$expression nstate))
	      (rparen (//rparen nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-iteration-statement-while test stmt nstate))
	(.do (($do (//do state))
	      (stmt (/!statement nstate))
	      ($while (//while nstate))
	      (lparen (//lparen nstate))
	      (test (/$expression nstate))
	      (rparen (//rparen nstate))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-iteration-statement-while-do
		       test stmt nstate))
	(.do (($for (//for state))
	      (lparen (//lparen nstate))
	      #-nil
	      (expr0 ($/?  nstate #'/$expression))
	      #+nil
	      (expr0 (multiple-value-bind (expr nstate)
			 ($/?  nstate #'/$expression)
		       #+nil
		       (print `(/!iteration-statement "expression 0 accepted"
						      ,expr))
		       (values expr nstate)))
	      (semi0 (//semicolon nstate))
	      #-nil
	      (expr1 ($/? nstate #'/$expression))
	      #+nil
	      (expr1 (multiple-value-bind (expr nstate)
			 ($/?  nstate #'/$expression)
		       #+nil
		       (print `(/!iteration-statement "expression 1 accepted"
						      ,expr))
		       (values expr nstate)))
	      (semi1 (//semicolon nstate))
	      #-nil
	      (expr2 ($/? nstate #'/$expression))
	      #+nil
	      (expr2 (multiple-value-bind (expr nstate)
			 ($/?  nstate #'/$expression)
		       #+nil
		       (print `(/!iteration-statement "expression 2 accepted"
						      ,expr))
		       (values expr nstate)))
	      (rparen (//rparen nstate))
	      (stmt (/!statement nstate)))
	  (return-stmt :c-stmt-iteration-statement-for
		       (list expr0 expr1 expr2) stmt nstate))
	(.do (($for (//for state))
		(lparen (//lparen nstate))
		(decl (/>declaration nstate))
		(expr0 ($/?  nstate #'/$expression))
		(semi0 (//semicolon nstate))
		(expr1 ($/? nstate #'/$expression))
		(semi1 (//semicolon nstate))
		(stmt (/!statement nstate)))
	  (return-stmt :c-stmt-iteration-statement-for
		       (list (list decl expr0) expr1) stmt nstate))))

(defun /!jump-statement (state)
  (dprint state  '/!jump-statement)
  (.any (.do (($goto (//goto state))
	      (ident (//identifier* nstate))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-jump-statement-goto ident nil nstate))
	(.do (($continue (//continue state))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-jump-statement-continue nil nil nstate))
	(.do (($break (//continue state))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-jump-statement-break nil nil nstate))
	(.do (($return (//return state))
	      (expr ($/? nstate #'/$expression))
	      (semi (//semicolon nstate)))
	  (return-stmt :c-stmt-jump-statement-return expr nil nstate))))

(defstruct (c-func-declaration (:include c-declaration
				(props :c-function-declaration))
			       (:conc-name func-) (:constructor mkfunc))
  (spec nil :type t)
  (oarg nil :type t))			;old type arguments


;;;
;;; external definitions
;;;
(defun /!translation-unit (state)
  (dprint state  '/!translation-unit)
  (.do ((unit (/!many-enq state #'/!external-declaration)))
    (return-stmt :c-stmt-translation-unit unit nil nstate)))


(defun have-typedef (decl)
  (if (eq (decl-props decl) :c-decl-storage-class-specifier)
      (let ((token (decl-value decl)))
	(if (eq (cpp-token-props token) :c-keyword-typedef) t nil))
      nil))

(defun register-typedef (name-decl spec)
  (let ((name
	  (cpp-token-value (decl-value (decl-body (decl-value name-decl))))))
    (setf (gethash name *typedef-hash*) spec)))

(defvar *accepted-declarations* (make-array 16 :adjustable t :fill-pointer 0))

(defun try-register-typedef (stmt)
  (let ((spec (decl-value stmt)))
    (let ((name-list (decl-body stmt)))
      (let ((typedef
	      (find-if (lambda (item)
			 (and (eq (decl-props item)
				  :c-decl-storage-class-specifier)
			      (let ((token (decl-value item)))
				(eq (cpp-token-props token)
				    :c-keyword-typedef))))
		       (decl-value spec))))
	(when typedef
	  (loop :for name :across (decl-value name-list)
		:do (register-typedef name spec)))))))

(defun try-register-enumerotion (stmt)
  (let ((decl-spec (decl-value stmt)))
    (let ((spec-list (decl-value decl-spec)))
      (let ((enum-wapper
	      (find-if (lambda (elt)
			 (and (eq (decl-props elt)
				  :c-decl-type-specifier)
			      (typep (decl-value elt)
				     'c-declaration)
			      (eq (decl-props (decl-value elt))
				  :c-decl-enum-specifier)))
		       spec-list)))
	(when enum-wapper
	  (loop :for elt :across (decl-body (decl-value enum-wapper))
		:do (let ((token (decl-value elt)))
		      (setf (gethash (cpp-token-value token)
				     *enumerattor-hash*)
			    elt))))))))

(defun register-object (stmt)
  (when stmt
    (try-register-typedef stmt)
    (try-register-enumerotion stmt)))

(defun extract-struct-body (state &optional
				    (acc (make-array 16 :adjustable t
							:fill-pointer 0))
				    (rec nil)
				    (nest 0)
				    (cnt 0))
  (multiple-value-bind (token nstate) (token-input state)
    (cond
      ((eq (token-props token) :c-punctuator-rbrace)
       (decf nest)
       (vector-push-extend token acc)
       (if (= nest 0)
	   (values acc nstate)
	   (extract-struct-body nstate acc rec nest (1+ cnt))))
      ((eq (token-props token) :c-punctuator-lbrace)
       (incf nest)
       (setf rec t)
       (vector-push-extend token acc)
       (extract-struct-body nstate acc rec nest (1+ cnt)))
      ((null rec)
       (extract-struct-body nstate acc rec nest cnt))
      (t
       (vector-push-extend token acc)
       (extract-struct-body nstate acc rec nest (1+ cnt))))))

(defun retry-/>struct-declaration-list% (state)
  (let* ((tkn-idx (position-if (lambda (item)
				 (let ((props (cpp-token-props item)))
				   (or (eq props :c-keyword-struct)
				       (eq props :c-keyword-uniom)
				       (eq props :c-keyword-enum)
				       (eq props :identifier))))
			       (is-i state) :start (is-o state)))
	 (token (aref (is-i state) tkn-idx)))
    (cond
      ((eq (token-props token) :identifier)
       ;; (type-qualifier|storage-class)* identifier0
       ;;     (type-qualifier|storage-class)* identifier1
       ;; will be treated identifier0 as a typedef'ed type
       #+nil
       (progn
	 (print "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
	 (print `(:>>>> ,(cpp-token-value token))))
       (setf (gethash (cpp-token-value token) *typedef-hash*)
	     (make-dummy-entry))
       (retry-/>struct-declaration-list state))
      (t
       ;; statement start with
       ;;      $- (struct|union|enum) (tag)? '{' -$ 
       (let ((xnstate (mktis :i (is-i state) :o tkn-idx)))
	 (multiple-value-bind (_ nstate)
	     (.any (.do ((kwd (.any (//struct xnstate) (//union xnstate)
				    (//enum xnstate)))
			 (tag (.any (//identifier* nstate) (//tag-name nstate)))
			 (lbrace (//lbrace nstate)))
		     (values lbrace nstate))
		   (.do ((kwd (.any (//struct xnstate) (//union xnstate)
				    (//enum xnstate)))
			 (lbrace (//lbrace nstate)))
		     (values lbrace nstate)))
	   (declare (ignorable _))
	   (cond
	     ((null nstate)
	      ;; struct/union/enum without body
	      ;; re-parse with
	      ;;   $- (struct|union|enum) (tag?) idetifier ";" -$
	      ;; and continue to parse
	      (multiple-value-bind (_ nstate)
		  (.do ((kwd (.any (//struct xnstate) (//union xnstate)
				   (//enum xnstate)))
			(tag (.any (//identifier* nstate) (//tag-name nstate)))
			(id-list (/>identifier-list nstate))
			(semi (//semicolon nstate)))
		    (values semi nstate))
		(declare (ignorable _))
		(retry-/>struct-declaration-list nstate)))
	     (t
	      ;; struct/union/enum with body
	      ;; extract body
	      (multiple-value-bind (body nstate)
		  (extract-struct-body (mktis :i (is-i nstate)
					      :o (1- (is-o nstate))))
		(let ((nstate (mktis :i (subseq body 1 (1- (length body))))))
		  (cond
		    ((eq (token-props token) :c-keyword-enum) nil)
		    (t
		     (retry-/>struct-declaration-list nstate))))
		(multiple-value-bind (_ nstate)
		    (.do ((id (/>identifier-list nstate))
			  (semi (//semicolon nstate)))
		      (values semi nstate))
		  (declare (ignorable _))
		  (retry-/>struct-declaration-list nstate)))))))))))

(defun retry-/>struct-declaration-list (state)
  (cond
    ((or (null state) (>= (is-o state) (length (is-i state))))
     (values t state))
    (t
     (multiple-value-bind (decl nstate) (/>struct-declaration-list state)
       (declare (ignorable decl))

       (cond
	 ((>= (is-o nstate) (length (is-i nstate)))
	  (values nil nil))
	 (t
	  (retry-/>struct-declaration-list% nstate)))))))

(defun retry-struct/union (state)
  (declare (ignorable state))

  (let* ((body (extract-struct-body state))
	 (len (length body))
	 (inner (subseq body 1 (1- len)))
	 (tis (mktis :i inner)))

    (multiple-value-bind (_ __) (retry-/>struct-declaration-list tis)
      (declare (ignore _ __))

      (multiple-value-bind (decl nstate) (/>declaration state)
	(values decl nstate)))))

(defun make-dummy-entry ()
  (mkdecl :props :c-decl-declaration-specifiers
	  :value (vector
		  (mkdecl :props :c-decl-storage-class-specifier
			  :value (make-cpp-token :props :c-keyword-typedef
						 :value "typedef"))
		  (mkdecl :props :c-decl-type-specifier
			  :value (make-cpp-token :props :dummy-typedef-entry
						 :value nil)))))
			
(defun retry-with-force-typedef (state)
  (let* ((vec (is-i state))
	 (idx (is-o state)))
    (cond
      ((>= idx (length vec))
       (values nil nil))
      (t
       (let ((semi (position-if (lambda (item)
				  (eq (cpp-token-props item)
				      :c-punctuator-semicolon))
				vec :start idx)))
	 (unless semi
	   (warn "malformed input")
	   (return-from retry-with-force-typedef (values nil nil)))
	 (let* ((text (subseq vec idx (1+ semi)))
		(ident (position-if (lambda (item)
				      (eq (cpp-token-props item) :identifier))
				    text))
		(delim (position-if (lambda (item)
				      (or (eq (cpp-token-props item)
					      :c-punctuator-semicolon)
					  (eq (cpp-token-props item)
					      :c-punctuator-comma)))
				    text)))
	   (when (or (not ident) (< (- delim ident) 2))
	     (warn "ident ~S delim-ident ~D" ident (if ident
						       (- delim ident)
						       -1))
	     ;; cannot invoke type estimation
	     (return-from retry-with-force-typedef (values nil nil)))

	   (cond
	     ((find-if (lambda (item)
			 (let ((props (cpp-token-props item)))
			   (or (eq props :c-keyword-struct)
			       (eq props :c-keyword-union)
			       (eq props :c-keyword-enum))))
		       text)
	      #+nil
	      (dump-hash *typedef-hash*)
	      (multiple-value-bind (_ __) (retry-struct/union state)
		(declare (ignorable _ __))
		(setf *force-print-typedef-name* t)
		(multiple-value-bind (decl nstate) (/>declaration state)
		  (setf *force-print-typedef-name* nil)
		  (values decl nstate))))
	     (t
	      (let ((token (aref text ident)))
		(setf (gethash (cpp-token-value token) *typedef-hash*)
		      (make-dummy-entry)))
		(/>declaration state)))))))))

(defun /!external-declaration (state)
  (dprint state  '/!external-declaration)
  ;;(.do ((stmt (.any (/>declaration state) (/!function-definition state))))
  (multiple-value-bind (stmt nstate)
      (.any (/>declaration state) (/!function-definition state))
    (cond
      ((null stmt)
       (multiple-value-bind (stmt nstate) (retry-with-force-typedef state)
	 (register-object stmt)
	 (if stmt
	     (values stmt nstate)
	     (values nil nil))))
      (t
       (vector-push-extend stmt *accepted-declarations*)
       (typecase stmt
	 (c-declaration
	  (case (decl-props stmt)
	    (:c-declaration (register-object stmt)))))
       (values stmt nstate)))))

(defun /!function-definition (state)
  (dprint state  '/!function-definition)
  (.any (.do ((spec (/>declaration-specifiers state))
	      (decl (/>declarator nstate))
	      (oarg (/!declaration-list nstate)) ; old style func def
	      (stmt (/!compound-statement nstate)))
	  (values (mkfunc :spec spec :value decl :body stmt :oarg oarg)
		  nstate))
	(.do ((spec (/>declaration-specifiers state))
	      (decl (/>declarator nstate)) ; old style func def
	      (stmt (/!compound-statement nstate)))
	  (values (mkfunc :spec spec :value decl :body stmt :oarg nil)
		  nstate))))

(defun /!declaration-list (state)
  (dprint state  '/!declaration-list)
  (.do ((decl-list (/!many-enq state #'/>declaration)))
    (return-stmt :c-stmt-declaration-list decl-list nil nstate)))

(defun init-builtins ()
  (setf (gethash "__builtin_va_list" *typedef-hash*)
	(mkdecl :props :c-decl-builtin-type)))
