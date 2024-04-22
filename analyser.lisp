(in-package :c-anl)

(defparameter *in-function* nil)
(defparameter *label-hash* (make-hash-table :test #'equal))
(defparameter *name-hash* (make-hash-table :test #'equal))
(defparameter *tag-hash* (make-hash-table :test #'equal))

#+nil
(progn
  (defparameter *typedef-hash* (make-hash-table :test #'equal))
  (defparameter *extern-hash* (make-hash-table :test #'equal))
  (defparameter *global-hash* (make-hash-table :test #'equal))
  (defparameter *static-hash* (make-hash-table :test #'equal))
  (defparameter *stack-hash* (make-hash-table :test #'equal))
  (defparameter *thread-hash* (make-hash-table :test #'equal)))

(defvar *token-input* nil)
(defvar *parsed-tree* nil)
(defvar *rest-tokens* nil)

(defstruct (ac-base-type (:conc-name ac-base))
  (type nil :type (or keyword null)))

(defstruct (ac-unknown  (:conc-name ac-uk-) (:include ac-base-type))
  (name nil :type (or string null)))

(defstruct (ac-pointer (:conc-name ac-ptr-) (:constructor mkacptr))
  (qual nil :type (or array null)))

(defstruct (ac-scalar (:conc-name ac-sc-) (:include ac-base-type))
  (size 0 :type (unsigned-byte 32))
  (sign nil :type boolean)
  (float nil :type boolean))

(defstruct (ac-non-scalar (:conc-name ac-st-) (:include ac-base-type))
  (name nil :type (or string null)))

(defstruct (ac-initializer (:conc-name ac-init-) (:include ac-base-type))
  (value nil :type (or vector nil)))

(defstruct (ac-init-value (:conc-name ac-ivar-) (:include ac-base-type))
  (value nil :type t))

(defstruct (ac-array-index (:conc-name ac-ai-) (:include ac-base-type))
  (idx nil :type (or c-expression cpp-token (eql :*) null))
  (static nil :type boolean)
  (qual nil :type (or (array keyword 1) null)))

(defstruct (ac-variable (:conc-name ac-var-) (:constructor mkacvar))
  (name "" :type string)
  (scs nil :type (or keyword null))
  (spec nil :type (or ac-scalar vector  null))
  (qual nil :type (or array null))
  (ptr nil :type (or ac-pointer null))
  (fnspec nil :type (or array null))
  (align nil :type t)
  (idx nil :type (or array null))
  (init nil :type (or ac-initializer ac-init-value null)))

(defstruct (ac-funarg (:conc-name ac-farg-) (:constructor mkacfarg))
  (spec nil :type (or ac-scalar cpp-token null))
  (qual nil :type (or vector null))
  (ptr nil :type t)
  (name nil :type (or string null))
  (suffix nil :type (or vector null)))

(defstruct (ac-funarg-list (:conc-name ac-falist-) (:constructor mkacfalist))
  (args nil :type (or vector nil))
  (rest nil :type boolean))

(defstruct (ac-funptr (:conc-name ac-fptr-) (:constructor mkacfptr))
  (ptr nil :type (or ac-pointer null))
  (args nil :type (or ac-funarg-list null)))

(defstruct (ac-funcall (:conc-name ac-fcall-) (:constructor mkacfcall))
  (name nil :type (or string null))
  (args nil :type (or vector null))
  (va-arg nil :type boolean))

(defstruct (ac-function (:conc-name ac-func-) (:constructor mkacfunc))
  (scs nil :type (or keyword null))
  (type nil)
  (qual nil)
  (fn-spec nil)
  (ptr nil)
  (name "" :type string)
  (arg-list nil :type (or vector null))
  (k&r-args nil :type (or vector null))
  (body nil))

(defstruct (ac-struct-field (:conc-name ac-sf-) (:constructor mkacsf)
			    (:copier cpacsf))
  (spec nil :type (or ac-base-type null))
  (fn-spec nil :type (or vector null))
  (qual nil :type (or vector null))
  (ptr-spec nil :type (or ac-pointer null))
  (suffix nil :type (or vector null))
  (name nil :type (or vector string null))
  (bits nil :type (or (integer 0 63) null)))

(defstruct (ac-struct (:conc-name ac-struct-) (:constructor mkacstruct))
  (type nil :type (or (member :c-keyword-struct :c-keyword-union)  null))
  (tag nil :type (or string null))
  (fields nil :type (or (array ac-struct-field 1) null))
  (hash (make-hash-table :test #'equal)))

(defstruct (ac-enum-entry (:conc-name ac-enume-) (:constructor mkacenume))
  (name "" :type string)
  (value nil :type (or integer null)))
  
(defstruct (ac-enum (:conc-name ac-enum-) (:constructor mkacenum))
  (tag "" :type string)
  (items nil :type (or (array ac-enum-entry 1) null))
  (hash (make-hash-table :test #'equal)))

(defstruct (ac-atomic-type (:conc-name ac-atom-) (:constructor mkacatom))
  (type-list (make-array 1 :adjustable t :fill-pointer 0)))

(defstruct (ac-var-name (:conc-name ac-vname-) (:constructor mkacvname))
  (type nil :type (or (member (:func-name :func-ptr :var-name)) null))
  (name "" :type string)
  (args nil :type (or ac-funarg-list null)))

(defstruct (ac-declarator (:conc-name ac-decl-) (:constructor mkacdecl))
  (pointer nil :type (or ac-pointer null))
  (name nil :type (or ac-var-name null)))

(defstruct (ac-typedef (:conc-name ac-typedef-) (:constructor mkactypedef))
  (ts #() :type vector)
  (qs #() :type vector)
  (name nil :type (or ac-declarator null)))

(defstruct (ac-block (:conc-name ac-blk-) (:constructor %make-ac-block))
  (parent nil :type (or ac-block null))
  (children (make-array 2 :adjustable t :fill-pointer 0)  :type vector)
  ;; variable hashes
  #+nil (typedef (make-hash-table :test #'equal))
  #+nil (extern (make-hash-table :test #'equal))
  #+nil (global (make-hash-table :test #'equal))
  #+nil (static (make-hash-table :test #'equal))
  #+nil (stack (make-hash-table :test #'equal))
  #+nil (thread (make-hash-table :test #'equal))
  (label (make-hash-table :test #'equal))
  (names (make-hash-table :test #'equal))
  (tag (make-hash-table :test #'equal))

  ;; functions -- valid only in gloval block
  (funcs (make-hash-table :test #'equal))
  (stmts nil :type (or (vector c-expression) null)))

(defun make-ac-block ()
  (let ((blk (%make-ac-block)))
    (print `(:MMMMMMMM make-ac-block :@ 1
	     :children.length ,(length (ac-blk-children blk))))
    #+nil
    (break)
    blk))

(defvar *current-block* (make-ac-block))

(defmacro with-block (blk-name &body body)
  `(let* ((,blk-name (make-ac-block))
	  (*tag-hash* (ac-blk-tag ,blk-name))
	  (*name-hash* (ac-blk-names ,blk-name))
	  (*label-hash* (ac-blk-label ,blk-name))
	  (parent *current-block*)
	  (*current-block* ,blk-name))
     (vector-push-extend ,blk-name (ac-blk-children parent))
     (setf (ac-blk-parent ,blk-name) parent)
     ,@body))

(defun file-to-directive-list (file &optional (acc (makeq 'cpp-directive)))
  (with-open-file (s file :direction :input
			  :if-does-not-exist :error
			  :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (let ((buf (sb-ext::octets-to-string buf :external-format :utf-8)))
	(=cpp-file (mkcis :i buf) acc)))))

(defun preprocess (file &optional (builtin t))
  (setf cpp-directive::*cpp-macro-hash* (make-hash-table :test #'equal))
  (setf c-stmt::*typedef-hash* (make-hash-table :test #'equal))
  (setf c-stmt::*enumerattor-hash* (make-hash-table :test #'equal))
  (init-builtins)
  (let* ((builtin (if builtin
		      (file-to-directive-list "builtin.h")
		      (makeq 'cpp-directive)))
	 (directive-list (file-to-directive-list file builtin))
	 (token-list (=/parse-cpp-directive (mktis :i directive-list))))
    (loop :for token :across token-list
	  :collect (cpp-token-value token) :into list
	  :when (string= (cpp-token-value token) ";")
	    :collect (format nil ":file ~S :line ~D"
			     (cpp-token-file token) (cpp-token-line token))
	      :into list
	  :when (string= (cpp-token-value token) ";")
	    :collect (coerce '(#\Newline) 'string) :into list
	  :finally (format t "~%~{~A ~}" list))
    (setf *token-input* token-list)
    (multiple-value-bind (tree nstate)
	(/!translation-unit (mktis :i token-list))
      (setf *parsed-tree* tree
	    *rest-tokens* nstate)
      (values tree nstate))))

(defvar *indent* " ")

(defun iinc ()
  (setf *indent* (concatenate 'string *indent* " ")))

(defun idec ()
  (setf *indent* (subseq *indent* 1)))

(defun iprint (obj)
  (format t "~%~A~S" (subseq *indent* 1) obj))

(defvar *analysed-tree* (make-array 16 :adjustable 5 :fill-pointer 0))

(defun analyse (tree &optional (acc (make-array 16 :adjustable t
						   :fill-pointer 0)))
  #+nil
  (print `(:!!!!!!!! analyse :<<<< ))
  (typecase tree
    (c-statement			;do before c-declaration
     (analyse-stmt tree acc))
    (c-func-declaration
     (analyse-func tree acc))
    (c-declaration
     (analyse-decl tree acc))
    (c-expression
     (analyse-expr tree acc))))

(defun analyse-stmt (tree &optional (acc (make-array 16 :adjustable t
							:fill-pointer 0)))
  (declare (ignorable tree acc))
  (case (stmt-props tree)
    (:c-stmt-translation-unit
     #+nil
     (print `(:######## analyse-stmt :@ :c-stmt-translation-unit
	      :*current-block* ,(sb-kernel:get-lisp-obj-address *current-block*)))
     (with-block blk
       (loop :for elt :across (stmt-value tree)
	     :do (analyse elt acc))))))


(defun analyse-decl (tree &optional (acc (make-array 16 :adjustable t
							:fill-pointer 0)))
  (let ((props (decl-props tree)))
    (case props
      (:c-declaration
       (parse-declaration tree acc)))))

(defun analyse-func
    (tree &optional (acc (make-array 16 :adjustable t :fill-pointer 0)))
  (declare (ignorable acc))
  #-nil
  (print `(:!!!!!!!! analyse-func :@ :<<<<))
  (let ((spec (func-spec tree))
	(decl (func-value tree))
	(body (func-body tree)))
    (print `(:!!!!!!!! analyse-func :@ 1
	     :spec==> ,spec :decl==> ,decl :body==> ,body))
    (multiple-value-bind (sc ts tq fs as)
	(parse-declaration-specifiers (decl-value spec))
      (declare (ignorable fs as))
      #-nil
      (print `(:!!!!!!!! analyse-func :@ 2
	       :sc==> ,sc :ts==> ,ts :qs==> ,tq :fs==> .fs))
      (multiple-value-bind (name ptr args) (parse-name-list (vector decl))
	(let* ((blk (parse-complex-statement body))
	       (fn (mkacfunc :scs sc :type ts :qual tq :fn-spec fs
			     :ptr ptr :name name :arg-list args :body blk)))
	  #-nil
	  (print `(:!!!!!!!! analyse-func :@ 3 :fn==> ,fn))
	  (break)
	  )))))

#+nil
(defmacro with-block-bind (block &body body)
  `(let ((*typedef-hash* (ac-blk-typedef ,block))
	(*extern-hash* (ac-blk-extern ,block))
	(*global-hash* (ac-blk-global ,block))
	(*static-hash* (ac-blk-static ,block))
	(*stack-hash* (ac-blk-stack ,block))
	(*thread-hash* (ac-blk-thread ,block))
	(*tag-hash* (ac-blk-tag ,block)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((builtin '(:c-keyword-char :c-keyword-short :c-keyword-int
		   :c-keyword-long :c-keyword-signed :c-keyword-unsigned
		   :c-keyword-float :c-keyword-double :c-keyword-_Bool
		   "char" "short" "int" "long" "signed" "unsigned"
		   "float" "double" "_Bool")))
    (defun builtin-type-p (type)
      (member type builtin))))

(declaim (ftype (function ((or keyword cpp-token null)) t) resolv-typedef))
(defun resolv-typedef (type)
  (cond
    ((null type) nil)
    ((builtin-type-p type) type)
    (t
     (multiple-value-bind (type found) (gethash type *name-hash*)
       (break)
       (if (not found)
	   nil
	   (resolv-typedef type))))))

(defun identify-scalar-type** (specs)
  (print `(:-------- identify-scalar-type** :@ :<<<< :specs==> ,specs))
  (let ((char nil) (short nil) (int nil) (long nil) (signed t)
	(float nil) (double nil) (_Bool nil))
    (loop :for elt :across specs
	  :do (print `(:-------- identify-scalar-type** :@ :l0 :elt==> ,elt))
	      (case elt
		(:c-keyword-char (setf char t))
		(:c-keyword-short (setf short t))
		(:c-keyword-int (setf int t))
		(:c-keyword-long (setf long t))
		(:c-keyword-signed (setf signed t))
		(:c-keyword-unsigned (setf signed nil))
		(:c-keyword-float (setf float t))
		(:c-keyword-double (setf double t))
		(:c-keyword-_Bool (setf _Bool t))))
    (cond
      ((and long double)
       (make-ac-scalar :type :float :size 128 :sign nil :float t))
      ((or (and long int) long)
       (make-ac-scalar :type :integer :size 64 :sign signed :float nil))
      (int
       (make-ac-scalar :type :integer :size 32 :sign signed :float nil))
      (short
       (make-ac-scalar :type :integer :size 16 :sign signed :float nil))
      ((or char _Bool)
       (make-ac-scalar :type (if _Bool :boolean :integer)
		       :size 8 :sign signed :float nil))
      (double
       (make-ac-scalar :type :float :size 64 :sign nil :float t))
      (float
       (make-ac-scalar :type :float :size 32 :sign nil :float t))
      (t nil))))

(defun estimate-type* (specs)
  (let ((type (identify-scalar-type** specs)))
    (if type
	type
	(let ()
	  (print `(:-------- estimate-type* :@ 1 :specs==> ,specs))
	  (resolv-typedef (aref specs 0))))))

(defun hash-table-copy (hash)
  (let ((new-hash (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k new-hash) v)) hash)
    hash))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((unique-tag-counter 0))
    (defun unique-tag-name ()
      (let ((tag (format nil "$!tag-~D!$" unique-tag-counter)))
	(incf unique-tag-counter)
	tag))))

(defun print-block-nest (blk)
  (cond
    ((null blk) (format t ")"))
    ((eq blk *current-block*)
     (format t "~%(:PRINT-BLOCK-NEST==>~%(:SELF==> ~X :PARENT==> ~X ~
               :NAMES==> ~S :CHILDREN==> ~D)"
	     (sb-kernel:get-lisp-obj-address blk)
	     (sb-kernel:get-lisp-obj-address (ac-blk-parent blk))
	     (ac-blk-names blk) (length (ac-blk-children blk)))
     (print-block-nest (ac-blk-parent blk)))
    (t
     (format t "~%(:SELF==> ~X :PARENT==> ~X :NAMES==> ~S :CHILDREN==> ~D)"
	     (sb-kernel:get-lisp-obj-address blk)
	     (sb-kernel:get-lisp-obj-address (ac-blk-parent blk))
	     (ac-blk-names blk) (length (ac-blk-children blk)))
     (print-block-nest (ac-blk-parent blk)))))

(defun eval-structured-init (init &optional
				    (acc (make-array 4 :adjustable t
						       :fill-pointer 0))
				    (idx 0))
  (declare (ignorable acc idx))
  (print `(:%%%%%%%% eval-structured-init :@ :<<<< :init==> ,init))
  (break)
  init)

(defun bind-init-values (var init)
  (print `(:%%%%%%%% bind-init-values :@ :<<<< :var==> ,var :init==> ,init))
  (break)
  (when init
    (let* ((init (if (ac-var-idx var)
		     init
		     (aref (ac-init-value init) 0)))
	   (sc (ac-var-scs var))
	   (reduced-init (progn
			   (print `(:%%%%%%%% bind-init-values :@ 1 :sc==> ,sc))
			   (case sc
			     ((:c-keyword-typedef :c-keyword-auto
			       :c-keyword-register :allocate-stack)
			      init)
			     (t
			      (print `(:%%%%%%%% bind-init-values :@ 2
				       :init==> ,init))
			      (break)
			      (typecase init
				(c-expression
				 (let ((value (eval-constant-expr init)))
				   (make-ac-init-value :value value)))
				(cpp-token
				 (let ((value (cpp-token-value init)))
				   (make-ac-init-value :value value)))
				(ac-initializer
				 (eval-structured-init init))))))))
      (print `(:%%%%%%%% bind-init-values :@ 3
	       :reduced-init==> ,reduced-init))
      (setf (ac-var-init var) (make-ac-init-value :value reduced-init))
      (print `(:%%%%%%%% bind-init-values :@ :<<<< :var==> ,var)))))

(defun reduce-suffix (suffix &optional
			       (acc (make-array 1 :adjustable t
						  :fill-pointer 0))
			       (idx 0))
  (cond
    ((>= idx (length suffix))
     acc)
    (t
     (print `(:$$$$$$$$ reduce-suffix :@ :<<<< :suffix==> ,suffix))
     (break))))

(defun make-variable (nl  sc ts tq fs as &optional
					   (acc (make-array 2 :adjustable t
							      :fill-pointer 0))
					   (idx 0))
  (declare (ignorable acc idx))

  (print `(:$$$$$$$$ make-variable :@ :<<<< :nl==> ,nl))
  (cond
    ((>= idx (length nl))
     (print `(:$$$$$$$$ make-variable :@ :>>>>))
     (dump-hash* *name-hash* *standard-output*)
     (print-block-nest *current-block*)
     acc)
    (t
     (print `(:$$$$$$$$ make-variable :@ 1 :var==> ,(aref nl idx)))
     (let* ((var (aref nl idx))
	    (name (car var))
	    (init (cdr var))
	    (name-decl (car name))
	    (name-str (ac-decl-name name-decl))
	    (ptr (ac-decl-pointer name-decl))
	    (suffix (cdr name))
	    (sc (if sc
		    sc
		    (if *in-function* :allocate-stack  :allocate-global)))
	    (var (mkacvar :scs sc :spec ts :qual tq :fnspec fs :align as
			  :name name-str :ptr ptr :idx suffix)))
       (reduce-suffix suffix)
       (print `(:$$$$$$$$ make-variable :@ 2 :suffix==> ,suffix))
       (break)
       #+nil
       (print `(:$$$$$$$$ make-variable :@ 2 :hash==> ,*name-hash*
		:var==> ,var :name-str==> ,name-str :name==> ,name))
       (setf (gethash name-str *name-hash*) (vector sc var))
       (bind-init-values var init)
       (make-variable nl sc ts tq fs as acc (1+ idx))))))

(defun parse-declaration (decl &optional
				 (acc (make-array 1 :adjustable t
						    :fill-pointer 0))
				 (idx 0))
  (declare (ignorable acc idx))
  #-nil
  (print `(:++++++++ parse-declaration :@ :<<<< :tree==> ,decl))
  (let ((specs (decl-value decl))
	(decls (decl-body decl)))
    (multiple-value-bind (sc ts tq fs as)
	(parse-declaration-specifiers (decl-value specs))
      (print `(:++++++++ parse-declaration :@ 1
	       :sc==> ,sc :ts==> ,ts :tq==> ,tq :fs==> ,fs :as== ,as))
      (let ((nl (parse-name-list (and decls (decl-value decls)))))
	(print `(:++++++++ parse-declaration :@ 2 :nl==> ,nl))
	(break)
	(when nl
	  (make-variable nl sc ts tq fs as))))))

(defun parse-struct/union-decl (spec)
  #-nil
  (print `(:++++++++ parse-struct/union-decl :<<<< :spec==> ,spec))
  (let ((tag (if (decl-value spec)
		 (cpp-token-value (decl-value spec))
		 (unique-tag-name)))
	(type (case (decl-props spec)
		(:c-decl-struct-specifier :c-keyword-struct)
		(:c-decl-union-specifier :c-keyword-union)
		(t (error "~S called with unknown specifier ~S."
			  'parse-struct/union-decl (decl-props spec))))))
    (unless (decl-value spec)
      (setf (decl-value spec)
	    (make-cpp-token :props :identifier :value tag)))
    #-nil
    (print `(:++++++++ parse-struct/union-decl :@ 0
	     :tag==> ,tag :token==> ,(decl-value spec)
	     :decl==> ,(decl-body spec)))
    (let* ((fields (if (decl-body spec)
		       (parse-struct/union-body (decl-body spec))
		       nil))
	   (struct (mkacstruct :tag tag :type type :fields fields)))
      (multiple-value-bind (elt found) (gethash tag *tag-hash*)
	(if (not found)
	    (setf (gethash tag *tag-hash*) struct)
	    (let ((body (ac-struct-fields elt)))
	      (cond
		((and fields body)
		 (error "struct ~A already defined as ~S" tag elt))
		(fields (setf (gethash tag *tag-hash*) struct))
		(t nil)))))
      (vector (cons tag type) struct))))
  
(defun parse-pointer-spec* (spec &optional
				   (acc (make-array 2 :adjustable t
						      :fill-pointer 0))
				   (idx 0))
  (print `(:******** parse-pointer-spec* :@ :<<<< :spec==> ,spec))
  (if (null spec)
      (if (= (length acc) 0)
	  nil
	  (mkacptr :qual acc))
      (progn
	(unless (eq (decl-props spec) :c-decl-pointer)
	  (error "~S is not :C-DECL-POINTER" spec))
	(let ((type-list (decl-value spec)))
	  (let ((vec (loop :for elt :across (decl-value type-list)
			   :collect (let ((qual (decl-value elt)))
				      (cpp-token-props qual))
			     :into acc
			   :finally (return (coerce acc 'vector)))))
	    (vector-push-extend vec acc)))
	(parse-pointer-spec* (decl-body spec) acc (1+ idx)))))

(defun test-spec-len (spec)
  (cond
    ((vectorp spec)
     (if (= (length spec) 0)
	 nil
	 spec))
    (t
     spec)))

(defun dispatch-struct/union-enum-parser (specs idx)
  #+nil
  (print `(:++++++++ dispatch-struct/union-enum-parser :@ :<<<<
	   :idx ,idx :specs ,specs))
  (let ((spec (aref specs idx)))
    #+nil
    (print `(:++++++++ dispatch-struct/union-enum-parser :@ 0
	     :spec==> ,spec))
    (case (decl-props (decl-value spec))
      ((:c-decl-struct-specifier :c-decl-union-specifier)
       (let ((st/un (parse-struct/union-decl (decl-value spec))))
	 #+nil
	 (print `(:++++++++ dispatch-struct/union-enum-parser :@ 1
		  :st/un==> ,st/un))
	 st/un))
      (t
       (error "not implemented yet.")))))

(defun parse-declaration-specifiers
    (specs &optional
	     (sc nil)
	     (ts (make-array 1 :adjustable t :fill-pointer 0))
 	     (tq (make-array 1 :adjustable t :fill-pointer 0))
	     (fs (make-array 1 :adjustable t :fill-pointer 0))
	     (as (make-array 1 :adjustable t :fill-pointer 0))
	     (idx 0))
  #+nil
  (print `(:++++++++ parse-declaration-specifiers :@ :<<<< :specs==> ,specs))
  (if (>= idx (length specs))
      (progn
	#-nil
	(print `(:++++++++ parse-declaration-specifiers :@ :q-1 :ts==> ,ts))
	(let (#+nil(ts (test-spec-len ts))
	      (ts (if (and ts (keywordp (aref ts 0)))
		      (estimate-type* ts)
		      (aref ts 0)))
	      (tq (test-spec-len tq))
	      (fs (test-spec-len fs))
	      (as (test-spec-len as)))
	  #-nil
	  (print `(:++++++++ parse-declaration-specifiers :@ :>>>>
		   :sc==> ,sc :ts==> ,ts :tq==> ,tq :fs==> ,fs :as==> ,as))
	  (values sc ts tq fs as)))
      (let ((spec (aref specs idx)))
	(print `(:++++++++ parse-declaration-specifiers :@ 0
		 :decl-props==> ,(decl-props spec)))
	(case (decl-props spec)
	  (:c-decl-storage-class-specifier
	   (setf sc (cpp-token-props (decl-value spec))))
	  (:c-decl-type-specifier
	   (print `(:++++++++ parse-declaration-specifiers :@ :type-spec
		    :spec==> ,spec))
	   (typecase (decl-value spec)
	     (cpp-token
	      (if (builtin-type-p (cpp-token-props (decl-value spec)))
		  (vector-push-extend (cpp-token-props (decl-value spec)) ts)
		  (let ((val (decl-value spec)))
		    (print `(:++++++++ parse-declaration-specifiers :@ :token
			     :spec==> ,spec :val==> ,val))
		    (vector-push-extend (decl-value spec) ts))))
	     (c-declaration
	      (let ((st/un (dispatch-struct/union-enum-parser specs idx)))
		(print `(:++++++++ parse-declaration-specifiers :@ :st/un
			 :st/un==> ,st/un))
		(vector-push-extend st/un ts)))))
	  (:c-decl-type-qualifier
	   (vector-push-extend (cpp-token-props (decl-value spec)) tq))
	  (:c-decl-func-specifier
	   (vector-push-extend (cpp-token-props (decl-value spec)) fs))
	  (:c-decl-alignment-specifier
	   (vector-push-extend spec as)))
	(parse-declaration-specifiers specs sc ts tq fs as (1+ idx)))))

(defun parse-declarator* (decl)
  #+nil
  (print `(:@@@@@@@@ parse-declarator* :@ :<<<< :decl==> ,decl))
  (when decl
    (let* ((ptr (parse-pointer-spec* (decl-value decl)))
	   (name (decl-body decl))
	   (token (decl-value name))
	   (index (parse-suffixes* (decl-body name))))
      (declare (ignorable ptr name token index))
      #+nil
      (print `(:@@@@@@@@ parse-declarator* :@ 1 :ptr==> ,ptr
	       :name==> ,name :token==> ,token :index==> ,index))
      (values ptr token index))))

(defun parse-parameter-list
    (params want-id &optional
		      (acc (make-array 2 :adjustable t :fill-pointer 0))
		      (idx 0))
  (declare (ignorable want-id acc idx))

  #+nil
  (when (null params)
    (error "parse-parameter-list: NULL parameter list given."))
  #+nil
  (print `(:@@@@@@@@ parse-parameter-list :@ 0 :params==> ,params))
  (if (>= idx (length params))
      (progn
	#+nil
	(print `(:@@@@@@@@ parse-parameter-list :@ :exit
		 :length-params==> ,(length params) :idx==> ,idx :acc==> ,acc))
	acc)
      (let* ((arg (aref params idx)))
	(unless (eq (decl-props arg) :c-decl-parameter-declaration)
	  (error "~S is not parameter declaration" arg))
	(let ((spec (decl-value arg))
	      (decl (decl-body arg)))
	  (unless spec
	    (error "declatator ~S doues not have a specification of declatation"
		   arg))
	  (when (and want-id (null decl))
	    (error "no argument name in function desription"))
	  #-nil
	  (print `(:@@@@@@@@ parse-parameter-list
		   :@ 1 :spec==> ,spec :decl==> ,decl))
	  (multiple-value-bind (sc ts tq fs as)
	      (parse-declaration-specifiers (decl-value spec))
	    #-nil
	    (print `(:@@@@@@@@ parse-parameter-list :@ 2
		     :sc==> ,sc :ts==> ,ts :tq==> ,tq :fs==> ,fs :as==> ,as))
	    (when sc
	      (error "invalid storage class specifier in function declarator"))
	    (when fs
	      (error "~S can only appear on functions" fs))
	    (when as
	      (error "_Alignas attribute cannot to be applied to a function ~
                      parameter"))
	    #-nil
	    (print `(:******************************** parse-parameter-list))
	    #-nil
	    (print `(:@@@@@@@@ parse-parameter-list :@ 3
		     :spec==> ,spec :decl==> ,decl :lenhth==> ,(length params)
		     :idx==> ,idx :params==> ,params))
	    (multiple-value-bind (ptr name suffix) (parse-declarator* decl)
	      #-nil
	      (print `(:@@@@@@@@ parse-parameter-list :@ 4
		       :spec==> ,spec :ptr==> ,ptr :name==> ,name
		       :suffix==> ,suffix))
	      (vector-push-extend (mkacfarg :name (if name
						      (cpp-token-value name)
						      nil)
					    :spec ts :ptr ptr
					    :suffix suffix)
				  acc)
	      #+nil
	      (print `(:@@@@@@@@ parse-parameter-list :@ 5
		       :params==> ,params :want-id==> ,want-id
		       :acc==> ,acc :idx==> ,idx))
	      (parse-parameter-list params want-id acc (1+ idx))))))))

(defun %parse-suffixes* (suffixes &optional
				    (acc (make-array 2 :adjustable t
						       :fill-pointer 0))
				    (idx 0))
  #+nil
  (print `(:@@@@@@@@ %parse-suffixes* :@ :<<<<
	   :length==> ,(length suffixes) :idx==> ,idx :suffixes==> ,suffixes))
  (if (>= idx (length suffixes))
      (progn
	#+nil
	(print `(:@@@@@@@@ %parse-suffixes* :@ :>>>> :acc==> ,acc))
	acc)
      (let ((suffix (aref suffixes idx)))
	(typecase suffix
	  (c-array-index
	   #+nil
	   (print `(:@@@@@@@@ %parse-suffixes* :@ 1 :suffix==> ,suffix
			      :index==> ,(cai-index suffix)))
	   (let ((index (cai-index suffix)))
	     #+nil
	     (print `(:@@@@@@@@ %parse-suffixes* :let :l0-0
		      :value==> ,(decl-body suffix)))
	   (let ((qual (if (decl-body suffix)
			   (loop :for elt
				   :across (decl-value (decl-body suffix))
				 :collect (let ((token (decl-value elt)))
					    (cpp-token-props token))
				   :into acc
				 :finally (return (coerce acc 'vector)))
			   nil)))
	     #+nil
	     (print `(:@@@@@@@@ %parse-suffixes* :let 1 :qual==> ,qual
				:value==> ,(decl-value suffix)))
	   (let ((static (not (null (decl-value suffix)))))
	     #+nil
	     (print `(:@@@@@@@@ %parse-suffixes* :@ 2
		      :index==> ,index :qual==> ,qual :static==> ,static))
	     (vector-push-extend (make-ac-array-index :idx index :qual qual
						      :static static)
				 acc)
	     #+nil
	     (print `(:@@@@@@@@ %parse-suffixes* :@ 2 :acc==> ,acc))))))
	  (c-declaration
	   #+nil
	   (print `(:@@@@@@@@ %parse-suffixes* :@ 3 :suffix==> ,suffix))
	   (unless (eq (decl-props suffix) :c-function-arguments)
	     (error "~S is not fumction argument specifier" suffix))
	   (let* ((params (decl-value suffix))
		  (rest-args (decl-body params))
		  (ptype-list (decl-value params)))
	     #+nil
	     (print `(:@@@@@@@@ %parse-suffixes* :@ 3
		      :ptype-list==> ,ptype-list))
	     (let ((param-list (decl-value ptype-list)))
	       #+nil
	       (print `(:@@@@@@@@ %parse-suffixes* :@ 4
			:params-list==> ,param-list :rest-args==> ,rest-args))
	       (let ((args (parse-parameter-list param-list nil))
		     (rest-args (not (null rest-args))))
		 #+nil
		 (print `(:@@@@@@@@ %parse-suffixes* :@ 5 :args==> ,args))
		 #+nil
		 (break)
		 (vector-push-extend (mkacfalist :args args :rest rest-args)
				     acc))))))
	(%parse-suffixes* suffixes acc (1+ idx)))))
       
(defun parse-suffixes* (suffixes)
  (when (and suffixes (> (length suffixes) 0))
    #+-nil
    (print `(:%%%%%%%% parse-suffixes* :@ 0 :suffixes==> ,suffixes))
    (%parse-suffixes* suffixes)))

(defun parse-name-spec* (name)
  #+nil
  (print `(://////// parse-name-spec* :@ 0 :name==> ,name))
  (let* ((name-node (decl-value name))
	 (name-decl (typecase name-node
		      (cpp-token (cpp-token-value name-node))
		      (c-declaration
		       (let* ((ptr (decl-value name-node))
			      (name (decl-body name-node))
			      (ptr-spec (parse-pointer-spec* ptr)))
			 (vector (cpp-token-value (decl-value name))
				 ptr-spec)))))
	 (suffix (parse-suffixes* (decl-body name))))
    #+nil
    (print `(://////// parse-name-spec* :@ 1 :name-decl==> ,name-decl
	     :suffix==> ,suffix))
    (values name-decl suffix)))

(defun parse-struct-declarator (decl)
  #+nil
  (print `(:!!!!!!!! parse-struct-declarator :@ 0 :decl==> ,decl))
  (unless (eq (decl-props decl) :c-decl-struct-declarator)
    (error "~S is not a struct declarator" decl))
  (let* ((declarator (decl-value decl))
	 (ptr-spec (parse-pointer-spec* (decl-value declarator))))
    #+nil
    (print `(://////// parse-struct-declarator :@ 1 :ptr-spec==> ,ptr-spec))
    (multiple-value-bind (name suffix) (parse-name-spec* (decl-body declarator))
      #+nil
      (print `(://////// parse-struct-declarator :@ 2
	       :name==> ,name :suffix==> ,suffix))
      (values ptr-spec name suffix))))
	
(defun allocate-named-body (names template acc &optional (idx 0))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (if (>= idx (length names))
      acc
      (let ((name (aref names idx))
	    (field (cpacsf template)))
	#-nil
	(print `(:++++++++ allocate-named-body :@ :<<<<
		 :idx==> ,idx :name==> ,name))
	#+nil
	(break)
	(multiple-value-bind (ptr name sfx) (parse-struct-declarator name)
	  #+nil
	  (print `(://////// allocate-named-body :@ 1 :template==> ,template
		   :ptr==> ,ptr :name==> ,name :sfx==> ,sfx))
	  (setf (ac-sf-ptr-spec field) ptr
		(ac-sf-suffix field) sfx
		(ac-sf-name field) name)
	  (vector-push-extend field acc)
	  #+nil
	  (print `(://////// allocate-named-body :@ 2 :field==> ,field)))

	(allocate-named-body names template acc (1+ idx)))))

(defun make-qualifiers-list (ql)
  (if ql
      (loop :for elt :across ql
	    :collect (cpp-token-props elt) :into acc
	    :finally (return (coerce acc 'vector)))
      nil))

(defun parse-struct/union-body (body &optional
				 (acc (make-array 4 :adjustable t
						    :fill-pointer 0))
				 (idx 0))
  (if (>= idx (length body))
      acc
      (let* ((field (aref body idx))
	     (specs (decl-value field))
	     (names (decl-body field)))
	(declare (ignorable names))
	#-nil
	(print `(:@@@@@@@@ parse-struct-body :@ 0
		 :specs==> ,specs :names==> ,names))
	(multiple-value-bind (sc sl ql fs as)
	    (parse-declaration-specifiers (decl-value specs))
	  (declare (ignorable sc fs as))
	  (let* ((qual-list (make-qualifiers-list ql)))
	    #-nil
	    (print `(:++++++++ parse-struct-body :@ 1 :qual-list==> ,qual-list))
	    (let ((template (mkacsf :spec sl :qual ql)))
	      (print `(:++++++++ parse-struct-body :@ 2 :template==> ,template))
	      (unless (eq (decl-props names) :c-decl-struct-declarator-list)
		(error "~S is not a :c-decl-struct-declarator" names))
	      (allocate-named-body (decl-value names) template acc)
	      (parse-struct/union-body body acc (1+ idx))))))))

(defun parse-spec-qual-list*
    (spec-list &optional
		 (idx 0)
		 (sc nil)
		 (tl (make-array 1 :adjustable t :fill-pointer 0))
		 (ql (make-array 1 :adjustable t :fill-pointer 0)))
  (declare (ignorable idx sc tl ql))
  (print `(:-------- parse-spec-qual-list* :@ :<<<< :spec-list==> ,spec-list))
  (if (>= idx (length spec-list))
      (values sc tl ql)
      (let ((decl (aref spec-list idx)))
	(print `(:-------- parse-spec-qual-list* :@ 1 :decl==> ,decl))
	(case decl
	  (:c-decl-storage-class-specifier
	   (when sc
	     (error "mutiple storage-class specifier ~S and ~S" sc decl))
	   (setf sc decl))
	  (:c-decl-type-qualifier
	   (vector-push-extend decl ql))
	  (:c-decl-type-specifier
	   (vector-push-extend decl tl)))
	(parse-spec-qual-list* spec-list (1+ idx) sc tl ql))))

(defun construct-funptr (decl suffixes)
  (print `(:++++++++ construct-funptr :@ :<<<<
	   :decl==> ,decl :suffixes==> ,suffixes))
  (let* ((funarg (aref suffixes 0))
	 (indexes (subseq suffixes 1))
	 (name-decl (decl-body decl))
	 (name-token (decl-value name-decl)))
    (print `(:++++++++ construct-funptr :@ 1
	     :funarg==> ,funarg :indexes==> ,indexes
	     :name-decl==> ,name-decl :name-token==> ,name-token))
    (values (mkacvname :type :func-ptr :name (cpp-token-value name-token)
		       :args funarg)
	    indexes)))

(defun parse-funptr-declarator (name-decl suffixes)
  (print `(:++++++++ parse-funptr-declarator :@ :<<<<
	   :name-decl==> ,name-decl))
  (unless (eq (decl-props name-decl) :c-decl-declarator)
    (error "parse-funptr-declarator ~S is not :c-decl-declarator" name-decl))
  (let ((ptr (parse-pointer-spec* (decl-value name-decl)))
	(token (decl-value (decl-body name-decl)))
	(funarg (parse-parameter-list nil (aref suffixes 0)))
	(rest-suffixes (subseq suffixes 1)))
    (let ((funptr (mkacfptr :ptr ptr :args funarg)))
      (print `(:++++++++ parse-funptr-declarator :@ 1
	       :funptr==> ,funptr :rest-suffixes==> ,rest-suffixes))
      (break)
      (values token funptr rest-suffixes))))


(defun parse-declarator (name init)
  (let ((ptr (parse-pointer-spec* (decl-value name)))
	(name-spec (decl-body name)))
    (print `(:++++++++ parse-declarator :@ :<<<<
	     :ptr==> ,ptr :name-spec==> ,name-spec))
    (unless (eq (decl-props name-spec) :c-decl-direct-declarator)
      (error "parse-declarator ~S is not :c-decl-direct-declarator" name-spec))
    (let ((name-decl (decl-value name-spec)))
      (print `(:++++++++ parse-declarator :@1 :name-decl==> ,name-decl))
      (break)
      (typecase name-decl
	(cpp-token name-decl)
	(c-declaration
	 (parse-funptr-declarator name-decl (decl-body name-spec)))
	(t
	 (error "unknown declarator type ~S" name-decl))
	))))


(defun parse-name-list (name-list &optional
				    (idx 0)
				    (acc (make-array 1 :adjustable t
						       :fill-pointer 0)))
  (cond
    ((null name-list) nil)
    ((>= idx (length name-list))
     (print `(:++++++++ parse-name-list :@ :>>>> :acc==> ,acc))
     acc)
    (t
     (let ((name-init (aref name-list idx)))
       (print `(:++++++++ parse-name-list :@ :<<<< :name-init==> ,name-init))
       (unless (eq (decl-props name-init) :c-decl-init-declarator)
	 (error "name-list includes an illegal entry ~S" name-init))
       (let ((name (decl-value name-init))
	     (init (decl-body name-init)))
	 (print `(:++++++++ parse-name-list :@ 1 :name==> ,name :init==> ,init))
	 (case (decl-props name)
	   (:c-decl-declarator
	    (parse-declarator name init))
	   (t
	    (error "unknown declarator-type ~S" (decl-props name))))
	 (break))))))


(defun parse-designator-list (elts &optional
				     (acc (make-array 1 :adjustable t
							:fill-pointer 0))
				     (idx 0))
  #+nil
  (print `(:@@@@@@@@ parse-designator-list :@ :<<<< :elts==> ,elts))
  (cond
    ((>= idx (length elts))
     #+nil
     (print `(:@@@@@@@@ parse-designator-list :@ :>>>> :acc==> ,acc))
     #+nil (break)
     acc)
    (t
     (let ((elt (aref elts idx)))
       #+nil
       (print `(:@@@@@@@@ parse-designator-list :@ 1 :elt==> ,elt))
       (unless (eq (decl-props elt) :c-decl-designator)
	 (error "Unexpected props ~S" (decl-props elt)))
       (let ((type (decl-value elt))
	     (val (decl-body elt)))
	 (case (cpp-token-props type)
	   (:c-punctuator-dot
	    (vector-push-extend (cons :field-designator val) acc))
	   (:c-punctuator-lbraket
	    (vector-push-extend (cons :index-designator val) acc))
	   (t
	    (error "Unknown designator type ~S" (cpp-token-value type))))
	 (parse-designator-list elts acc (1+ idx)))))))


(defun parse-designation (decl)
  #+nil
  (print `(:@@@@@@@@ parse-designation :@ :<<<< :decl==> ,decl))
  (cond
    ((null decl) nil)
    (t
     (let* ((list (decl-value decl))
	    (elts (decl-value list)))
       (parse-designator-list elts)))))

(defun parse-initializer-list (list &optional
				      (acc (make-array 4 :adjustable t
							 :fill-pointer 0))
				      (idx 0))
  (declare (ignorable list acc idx))

  #+nil
  (print `(:@@@@@@@@ parse-initializer-list :<<<< :list==> ,list))
  (cond
    ((>= idx (length list))
     #+nil
     (print `(:@@@@@@@@ parse-initializer-list :>>>> :acc==> ,acc))
     #+nil (break)
     acc)
    (t
     (let ((elt (aref list idx)))
       (unless (eq (decl-props elt) :c-decl-initializer-list-element)
	 (error "parse-initializer-list Unexpected props ~S" (decl-props elt)))
       (let ((var (parse-designation (decl-value elt)))
	     (val (decl-body elt)))
	 (declare (ignorable var val))
	 #+nil
	 (print `(:@@@@@@@@ parse-initializer-list :@ 1 :decl-body==> ,val))
	 (if (eq (decl-props val) :c-decl-initializer)
	     (let ((init (parse-initializer val)))
	       #+nil
	       (print `(:@@@@@@@@ parse-initializer-list :@ 2 :init==> ,init))
	       (vector-push-extend (cons var init) acc))
	     (vector-push-extend (cons var val) acc))
	 (parse-initializer-list list acc (1+ idx)))))))

(defun parse-initializer (elt)
  #-nil
  (print `(:@@@@@@@@ parse-initializer :@ :<<<< :elt==> ,elt))
  (cond
    ((null elt) nil)
    (t
     (unless (eq (decl-props elt) :c-decl-initializer)
       (error "Unknown initializer type ~S" (decl-props elt)))
     (let ((val (decl-value elt)))
       (typecase val
	 (cpp-token
	  (make-ac-initializer :value (vector val)))
	 (c-expression
	  (make-ac-initializer :value (vector val)))
	 (c-declaration
	  #+nil
	  (print `(:@@@@@@@@ parse-initializer :@ :c-declaration :val==> ,val))
	  (unless (eq (decl-props val) :c-decl-initializer-list)
	    (error "Unexpected property type ~S" (decl-props val)))
	  (let ((init-list (parse-initializer-list (decl-value val))))
	    (make-ac-initializer :value init-list)))
	 (t (error "Unknown initializer type ~S" val)))))))

(defun parse-alignement (spec)
  #+nil
  (print `(:======== parse-alignement :spec ,spec))
  (let ((align-type (decl-value spec))
	(align-body (decl-body spec)))
    (case align-type
      (:typename
       (let ((align-types (decl-value align-body))
	     (vec (make-array 1 :adjustable 1 :fill-pointer 0)))
	 (loop :for elt :across (decl-value align-types)
	       :do (let ((type (decl-value elt)))
		     (vector-push-extend (cpp-token-value type) vec)))
	 `(:typename ,vec)))
      (:constant
       (let ((val (cpp-eval-expr align-body)))
	 `(:constant ,val))))))

(let ((unique-field-counter 0))
  (defun unique-field-name ()
    (let ((field (format nil "$!field~D!$" unique-field-counter)))
      (incf unique-field-counter)
      field)))

(defun atomic-type (spec)
  #+nil
  (print `(atomic-type ,spec))
  (let* ((type-name (decl-value spec))
	 (spec-qual (decl-value type-name))
	 (spec-list (decl-value spec-qual))
	 (atom (mkacatom)))
    (loop :for elt :across spec-list
	  :do (let ((token (decl-value elt)))
		(vector-push-extend (cpp-token-value token)
				    (ac-atom-type-list atom))))
    #+nil
    (print atom)
    atom))

(defun find-variables (decl)
  (print `(:!!!!!!!! find-variables :@ :<<<< :decl==> ,decl))
  (break))

(defun parse-for-statement (stmt)
  #+nil
  (print `(:!!!!!!!! parse-for-statement :@ :<<<< :stmt==> ,stmt))
  (let* ((loop-var (decl-value stmt))
	 (loop-body (decl-body stmt))
	 (loop-start (car loop-var))
	 (loop-test (cadr loop-var))
	 (loop-next (caddr loop-var)))
    (print `(:!!!!!!!! parse-for-statement :@ 0
	     :loop-var==> (:start==> ,loop-start :test==> ,loop-test
			   :next==> ,loop-next)
	     :loop-body==> ,loop-body))
    (let ((vars (if (and (typep loop-start 'c-declaration)
			 (eq (decl-props loop-start) :c-declaration))
		    (list (parse-declaration loop-start))
		    (find-variables loop-start))))
      (print `(:!!!!!!!! parse-for-statement :@ 1 :vars==> ,vars))
      (break))))

(defun parse-c-statement (stmt)
  #+nil
  (print `(:!!!!!!!! parse-c-statement :@ :<<<< :stmt==> ,stmt))
  (let ((type (decl-props stmt)))
    (print `(:!!!!!!!! parse-c-statement :@ :<<<< :stmt==> ,type))
    (case type
      #+nil
      (:c-stmt-block-item)
      (:c-stmt-compound-statement)
      (:c-stmt-declaration-list)
      (:c-stmt-expression-statement)
      (:c-stmt-iteration-statement-for
       (parse-for-statement stmt))
      (:c-stmt-iteration-statement-while)
      (:c-stmt-iteration-statement-while-do)
      (:c-stmt-jump-statement-break)
      (:c-stmt-jump-statement-continue)
      (:c-stmt-jump-statement-goto)
      (:c-stmt-jump-statement-return)
      (:c-stmt-labeled-statement)
      (:c-stmt-labeled-statement-case)
      (:c-stmt-labeled-statement-default)
      (:c-stmt-selection-statement-switch)
      (:c-stmt-statement)
      (t (print `(:!!!!!!!! parse-c-statement :@ :case-t :type==> ,type))))))

(defun %parse-complex-statement (stmts &optional
					 (acc (make-array 16 :adjustable t
							     :fill-pointer 0))
					 (idx 0))
  (if (>= idx (length stmts))
      (progn
	(print `(:!!!!!!!! %parse-complex-statement :@ :>>>> :acc==> ,acc))
	(break)
	acc)
      (let* ((stmt (aref stmts idx))
	     (stmt/expr (decl-value stmt)))
	#+nil
	(print `(:!!!!!!!! %parse-complex-statement :@ 1 :stmt==> ,stmt
		 :stmt/expr==> ,stmt/expr))
	(typecase stmt/expr
	  (c-statement
	   (print `(:!!!!!!!! %parse-complex-statement :@ :c-statement))
	   (parse-c-statement (decl-value stmt/expr)))
	  (c-declaration
	   (print `(:!!!!!!!! %parse-complex-statement :@ :c-declaration))
	   (parse-declaration stmt/expr))
	  #+nil
	  (c-expression
	   (print `(:!!!!!!!! %parse-complex-statement :@ :c-expression)))
	  (t
	   (print `(:!!!!!!!! %parse-complex-statement :@ :otherwise))))
	(%parse-complex-statement stmts acc (1+ idx)))))

(defun parse-complex-statement (stmt)
  (with-block blk
    (print `(:!!!!!!!! parse-complex-statement :@ :<<<< :stmt==> ,stmt))
    (dump-hash* *tag-hash* *standard-output*)
    (dump-hash* *name-hash* *standard-output*)
    (dump-hash* *label-hash* *standard-output*)
    (%parse-complex-statement (decl-value stmt))
    (break)))

