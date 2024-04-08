(in-package :c-anl)

(defun file-to-directive-list (file &optional (acc (makeq 'cpp-directive)))
  (with-open-file (s file :direction :input
			  :if-does-not-exist :error
			  :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (let ((buf (sb-ext::octets-to-string buf :external-format :utf-8)))
	(=cpp-file (mkcis :i buf) acc)))))

(defvar *label-hash* (make-hash-table :test #'equal))
(defvar *tag-hash* (make-hash-table :test #'equal))
(defvar *typedef-hash* (make-hash-table :test #'equal))
(defvar *name-hash* (make-hash-table :test #'equal))

(defvar *token-input* nil)
(defvar *parsed-tree* nil)
(defvar *rest-tokens* nil)

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
     (loop :for elt :across (stmt-value tree)
	   :do (analyse elt acc)))))


(defun analyse-decl (tree &optional (acc (make-array 16 :adjustable t
							:fill-pointer 0)))
  (let ((props (decl-props tree)))
    (case props
      (:c-declaration
       (parse-declaration tree acc)))))

(defun analyse-func
    (tree &optional (acc (make-array 16 :adjustable t :fill-pointer 0)))
  (declare (ignorable acc))
  #+nil
  (print `(:!!!!!!!! analyse-func :<<<<))
  (let ((spec (func-spec tree))
	(decl (func-decl tree))
	(body (func-value tree)))
    (declare (ignorable body))
    (multiple-value-bind (sc ts qs) (parse-spec-qual-list (decl-value spec))
      #+nil
      (print `(:!!!!!!!! analyse-func :@ :sc==> ,sc :ts==> ,ts :qs==> ,qs))
      (let ((nl (parse-name-list (vector decl))))
	#+nil
	(print `(:!!!!!!!! analyse-func :@@ :nl==> ,nl))
	(let ((blk (parse-complex-statement body)))
	  (list sc ts qs nl blk))))))

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

(defstruct (ac-array-index (:conc-name ac-ai-) (:include ac-base-type))
  (idx nil :type (or c-expression cpp-token (eql :*) null))
  (static nil :type boolean)
  (qual nil :type (or (array keyword 1) nil)))

(defstruct (ac-variable (:conc-name ac-var-) (:constructor mkacvar))
  (name "" :type string)
  (scs nil :type (or keyword null))
  (spec nil :type (or ac-scalar null))
  (qual nil :type (or array null))
  (ptr nil :type (or ac-pointer null))
  (idx nil :type (or array null))
  (init nil :type t))

(defstruct (ac-funarg (:constructor ac-farg-) (:constructor mkacfarg))
  (name nil :type (or string null))
  (spec nil :type (or vector null))
  (qual nil :type (or vector null)))

(defstruct (ac-funcall (:conc-name ac-fcall-) (:constructor mkacfcall))
  (name nil :type (or string null))
  (args nil :type (or vector null))
  (va-arg nil :type boolean))

(defstruct (ac-function (:conc-name ac-func-) (:constructor mkacfunc))
  (scs nil :type (or keyword null))
  (type nil)
  (qual nil)
  (ptr nil)
  (name "" :type string)
  (arg-list nil :type (or vector null))
  (k&r-args nil :type (or vector null))
  (body nil))

(defstruct (ac-struct-field (:conc-name ac-sf-) (:constructor mkacsf))
  (spec nil :type (or ac-base-type null))
  (qual nil :type (or vector null))
  (align nil :type (or vector null))
  (ptr-spec nil :type (or ac-pointer null))
  (name nil :type (or string null))
  (bits nil :type (or (integer 0 63) null)))

(defstruct (ac-struct (:conc-name ac-struct-) (:constructor mkacstruct))
  (type nil :type (or keyword nil))
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

(defstruct (ac-declarator (:conc-name ac-decl-) (:constructor mkacdecl))
  (pointer nil :type (or ac-pointer null))
  (name "" :type string))
(defstruct (ac-typedef (:conc-name ac-typedef-) (:constructor mkactypedef))
  (ts #() :type vector)
  (qs #() :type vector)
  (name nil :type (or ac-declarator null)))

#-nil
(defun identify-scalar-type (spec-list)
  #+nil
  (print `(:++++++++ identify-scalar-type :<<<< :spec-list ,spec-list))
  (let ((char nil) (short nil) (int nil) (long nil) (signed t)
	(float nil) (double nil) (_Bool nil) (typedef nil))
    (if (typep spec-list 'list)
	(loop :for elt :in spec-list
	      :do (case (cpp-token-props elt)
		    (:c-keyword-char (setf char t))
		    (:c-keyword-short (setf short t))
		    (:c-keyword-int (setf int t))
		    (:c-keyword-long (setf long t))
		    (:c-keyword-signed (setf signed t))
		    (:c-keyword-unsigned (setf signed nil))
		    (:c-keyword-float (setf float t))
		    (:c-keyword-double (setf double t))
		    (:c-keyword-_Bool (setf _Bool t))
		    (:typedef-name (setf typedef (cpp-token-value elt)))))
	(loop :for elt :across spec-list
	      :do (case (cpp-token-props (decl-value elt))
		    (:c-keyword-char (setf char t))
		    (:c-keyword-short (setf short t))
		    (:c-keyword-int (setf int t))
		    (:c-keyword-long (setf long t))
		    (:c-keyword-signed (setf signed t))
		    (:c-keyword-unsigned (setf signed nil))
		    (:c-keyword-float (setf float t))
		    (:c-keyword-double (setf double t))
		    (:c-keyword-_Bool (setf _Bool t))
		    (:typedef-name (setf typedef
					 (cpp-token-value (decl-value elt)))))))
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
      (typedef
       #+nil
       (print `(:++++++++ identify-scalar-type :@ 1 :typedef==> ,typedef))
       #+nil
       (break)
       (multiple-value-bind (obj found) (gethash typedef *typedef-hash*)
	 (unless found
	   (setf obj (make-ac-unknown :name typedef))
	   (setf (gethash typedef *typedef-hash*) obj))
	 obj)))))

(let ((builtin  '(:c-keyword-char :c-keyword-short :c-keyword-int
		  :c-keyword-long :c-keyword-signed :c-keyword-unsigned
		  :c-keyword-float :c-keyword-double :c-keyword-_Bool
		  "char" "short" "int" "long" "signed" "unsigned"
		  "float" "double" "_Bool")))
  (defun builtin-type-p (type)
    (member type builtin)))

(declaim (ftype (function ((or keyword cpp-token null)) t) resolv-typedef))
(defun resolv-typedef (type)
  (cond
    ((null type) nil)
    ((builtin-type-p type) type)
    (t
     (multiple-value-bind (type found) (gethash type *typedef-hash*)
	 (if (not found)
	     nil
	     (resolv-typedef type))))))

#-nil
(defun identify-scalar-type* (spec-list)
  #+nil
  (print `(:++++++++ identify-scalar-type :<<<< :spec-list ,spec-list))
  (let ((char nil) (short nil) (int nil) (long nil) (signed t)
	(float nil) (double nil) (_Bool nil))
    (if (typep spec-list 'list)
	(loop :for elt :in spec-list
	      :do (case (cpp-token-props elt)
		    (:c-keyword-char (setf char t))
		    (:c-keyword-short (setf short t))
		    (:c-keyword-int (setf int t))
		    (:c-keyword-long (setf long t))
		    (:c-keyword-signed (setf signed t))
		    (:c-keyword-unsigned (setf signed nil))
		    (:c-keyword-float (setf float t))
		    (:c-keyword-double (setf double t))
		    (:c-keyword-_Bool (setf _Bool t))))
	(loop :for elt :across spec-list
	      :do (case (cpp-token-props (decl-value elt))
		    (:c-keyword-char (setf char t))
		    (:c-keyword-short (setf short t))
		    (:c-keyword-int (setf int t))
		    (:c-keyword-long (setf long t))
		    (:c-keyword-signed (setf signed t))
		    (:c-keyword-unsigned (setf signed nil))
		    (:c-keyword-float (setf float t))
		    (:c-keyword-double (setf double t))
		    (:c-keyword-_Bool (setf _Bool t)))))
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

(defun estimate-type (specs)
  (let ((type (identify-scalar-type* specs)))
    (if type
	type
	(resolv-typedef specs))))

#+nil
(defstruct (ac-block (:conc-name ac-blk-) (:constructor mkacblk)))

(defun hash-table-copy (hash)
  (let ((new-hash (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k new-hash) v)) hash)
    hash))

(let ((unique-tag-counter 0))
  (defun unique-tag-name ()
    (let ((tag (format nil "$!tag-~D!$" unique-tag-counter)))
      (incf unique-tag-counter)
      tag)))

(defun parse-declaration (tree &optional (acc (make-array 1 :adjustable t
							    :fill-pointer 0)))
  (declare (ignorable acc))
  #+nil
  (print `(:++++++++ parse-declaration :<<<< :tree=> ,tree))
  (let* ((specs (decl-value tree))
	 (decl (decl-body tree))
	 (names (and decl (decl-value decl))))
    (multiple-value-bind (sc td tl ql) (parse-spec-qual-list (decl-value specs))
      #+nil
      (print `(:++++++++ parse-declaration :@ 0
	       :sc==> ,sc :td==> ,td :tl==> ,tl :ql==> ,ql))
      (let ((nl (parse-name-list names)))
	#+nil
	(print `(:++++++++ :@@ parse-declaration :nl==> ,nl))
      (let ((st/un (find-if (lambda (elt)
			      (cond
				((and (eq (decl-props elt)
					  :c-decl-type-specifier)
				      (typep (decl-value elt) 'c-declaration))
				 (let ((props (decl-props (decl-value elt))))
				   (or (eq props :c-decl-struct-specifier)
				       (eq props :c-decl-union-specifier))))
				(t nil)))
			    tl)))
	#+nil
	(print `(:++++++++ :@@@ parse-declaration
			   :tl==> ,tl :struct/union==> ,st/un))
      (let ((have-suffix (and nl (aref (aref nl 0) 1))))
	#+nil
	(print `(:++++++++ :@@@ parse-declaration :have-suffix==> ,have-suffix))
      (let ((suffix-type (and have-suffix (aref (aref have-suffix 0) 0))))
	#+nil
	(print `(:++++++++ :@@@@ parse-declaration
			   :suffixe-type==> ,suffix-type))
	(cond
	  ((eq sc :c-keyword-typedef)
	   #+nil
	   (print `(:++++++++ :@@@@@ parse-declaration :c-keyword-typedef))
	   (when (or (null nl) (= (length nl) 0))
	     (error "typedef name not specified"))
	   (let ((typedef (mkactypedef :ts tl
				       :qs ql
				       :name have-suffix)))
	     (vector-push-extend typedef acc)))
	  (st/un
	   (let ((st/un (parse-struct/union-decl (decl-value st/un))))
	     (vector-push-extend st/un acc)))
	  ((eq suffix-type :c-function-arguments)
	   #+nil
	   (print `(:++++++++ :@@@@@@ parse-declaration :c-function-arguments))
	   (let ((func (mkacfunc :scs sc
				 :type tl
				 :qual ql
				 :ptr (ac-decl-pointer (aref (aref nl 0) 0))
				 :name (ac-decl-name (aref (aref nl 0) 0))
				 :arg-list have-suffix)))
	     (vector-push-extend func acc)))
	  (t
	   #+nil
	   (print `(:++++++++ parse-declaration :@ :tl==> ,tl))
	   (let ((type (identify-scalar-type tl)))
	     #+nil
	     (print `(:++++++++ parse-declaration :@@ :type==> ,type))
	     #+nil
	     (break)
	     #+nil
	     (print `(:++++++++ :@@@@@ parse-declaration :otherwise))
	     (loop :for elt :across nl
		   :do (let ((var (mkacvar :name (ac-decl-name (aref elt 0))
					   :scs sc
					   :spec type
					   :qual ql
					   :ptr (ac-decl-pointer (aref elt 0))
					   :idx (aref (aref nl 0) 1))))
			 (vector-push-extend var acc)))))))))))))

(defun parse-struct/union-decl (spec)
  #+nil
  (print `(:++++++++ parse-struct/union-decl :<<<< :struct==> ,struct))
  (let ((tag (if (decl-value spec)
		 (cpp-token-value (decl-value spec))
		 (unique-tag-name))))
    (unless (decl-value spec)
      (setf (decl-value spec)
	    (make-cpp-token :props :identifier :value tag)))
    #+nil
    (print `(:++++++++ parse-struct/union-decl :@
		       :tag==> ,tag :token==> ,(decl-value spec)))
    (parse-struct/union-body (decl-body spec))
    #+nil
    (break)))

(defun parse-pointer-spec* (spec &optional
				   (acc (make-array 2 :adjustable t
						      :fill-pointer 0))
				   (idx 0))
  (if (null spec)
      (progn
	(if (= (length acc) 0)
	    nil
	    (mkacptr :qual acc)))
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

(defun parse-declaration-specifiers
    (specs &optional (sc nil)
	     (ts (make-array 1 :adjustable t :fill-pointer 0))
 	     (tq (make-array 1 :adjustable t :fill-pointer 0))
	     (fs (make-array 1 :adjustable t :fill-pointer 0))
	     (as (make-array 1 :adjustable t :fill-pointer 0))
	     (idx 0))
  (print `(:++++++++ parse-declaration-specifiers :@ :<<<< :specs==> ,specs))
  (if (>= idx (length specs))
      (let ((ts (test-spec-len ts))
	    (tq (test-spec-len tq))
	    (fs (test-spec-len fs))
	    (as (test-spec-len as)))
	(print `(:++++++++ parse-declaration-specifiers :@ :>>>>
		 :sc==> ,sc :ts==> ,ts :tq==> ,tq :fs==> ,fs :as==> ,as))
	(break)
	(values sc ts tq fs as))
      (let ((spec (aref specs idx)))
	(case (decl-props spec)
	  (:c-decl-storage-class-specifier
	   (setf sc spec))
	  (:c-decl-type-specifier
	   (vector-push-extend spec ts))
	  (:c-decl-type-qualifier
	   (vector-push-extend spec tq))
	  (:c-decl-func-specifier
	   (vector-push-extend spec fs))
	  (:c-decl-alignment-specifier
	   (vector-push-extend spec as)))
	(parse-declaration-specifiers specs sc ts tq fs as (1+ idx)))))

(defun parse-declarator* (decl)
  (let ((ptr (parse-pointer-spec* (decl-value decl)))
	(name (decl-body decl))
	(token (decl-value name))
	(index (parse-index-in-param (decl-body name))))
    ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defun parse-parameter-list
    (params want-id &optional
		      (acc (make-array 2 :adjustable t :fill-pointer 0))
		      (idx 0))
  (declare (ignorable want-id acc idx))

  (when (null params)
    (error "parse-parameter-list: NULL parameter list given."))
  (print `(:@@@@@@@@ parse-parameter-list :@ 0 :params==> ,params))
  (if (>= idx (length params))
      (progn
	(print `(:@@@@@@@@ parse-parameter-list :@ :exit
		 :length-params==> ,(length params) :idx==> ,idx :acc==> ,acc))
	(break)
	acc)
      (let* ((arg (aref params idx)))
	(unless (eq (decl-props arg) :c-decl-parameter-declaration)
	  (error "~S is not parameter declaration" arg))
	(let ((spec (decl-value arg))
	      (desc (decl-body arg)))
	  (unless spec
	    (error "declatator ~S doues not have a specification of declatation"
		   arg))
	  (when (and want-id (null desc))
	    (error "no argument name in function desription"))
	  (print `(:@@@@@@@@ parse-parameter-list
		   :@ 1 :spec==> ,spec :decl==> ,desc))
	  (multiple-value-bind (sc ts tq fs as)
	      (parse-declaration-specifiers (decl-value spec))
	    (print `(:@@@@@@@@ parse-parameter-list :@ 2
		     :sc==> ,sc :ts==> ,ts :tq==> ,tq :fs==> ,fs :as==> ,as))
	    (when sc
	      (error "invalid storage class specifier in function declarator"))
	    (when fs
	      (error "~S can only appear on functions" fs))
	    (when as
	      (error "_Alifnas attribute cannot to be applied to a function ~
                      parameter"))
	    (print `(:******************************** parse-parameter-list))
	    (let ((spec (estimate-type ts))
		  (desc (parse-
	      (print `(:@@@@@@@@ parse-parameter-list :@ 3 :spec==> ,spec))
	      (
	      (break)))))))

(defun %parse-suffixes* (suffixes &optional
				    (acc (make-array 2 :adjustable t
						       :fill-pointer 0))
				    (idx 0))
  (print `(:@@@@@@@@ %parse-suffixes* :@ :<<<<
	   :length==> ,(length suffixes) :idx==> ,idx :suffixes==> ,suffixes))
  (if (>= idx (length suffixes))
      (progn
	(print `(:@@@@@@@@ %parse-suffixes* :@ :>>>> :acc==> ,acc))
	(break)
	acc)
      (let ((suffix (aref suffixes idx)))
	(print `(:@@@@@@@@ %parse-suffixes* :@ 1 :suffix==> ,suffix))
	(typecase suffix
	  (c-array-index
	   (let ((idx (cai-index suffix))
		  (qual (loop :for elt :across (decl-value (decl-value suffix))
			      :collect (let ((token (decl-value elt)))
					 (cpp-token-props token))
				:into acc
			      :finally (return (coerce acc 'vector))))
		  (static (eq (cpp-token-props (decl-body suffix))
			      :c-keyword-static)))
	     (print `(:@@@@@@@@ %parse-suffixes* :@ 2
		      :idx==> ,idx :qual==> ,qual :static==> ,static))
	     (vector-push-extend (make-ac-array-index :idx idx :qual qual
						      :static static)
				 acc)))
	  (c-declaration
	   (unless (eq (decl-props suffix) :c-function-arguments)
	     (error "~S is not fumction argument specifier" suffix))
	   (let* ((params (decl-value suffix))
		  (ptype-list (decl-value params)))
	     (print `(:@@@@@@@@ %parse-suffixes* :@ 3
		      :ptype-list==> ,ptype-list))
	     (let ((param-list (decl-value ptype-list))
		   (rest-args (decl-body ptype-list)))
	       (print `(:@@@@@@@@ %parse-suffixes* :@ 4
			:params-list==> ,param-list :rest-args==> ,rest-args))
	       (break)
	       (let ((args (parse-parameter-list param-list nil)))
		 (vector-push-extend args acc)
		 (when rest-args
		   (vector-push-extend rest-args acc)))))))
	(%parse-suffixes* suffixes acc (1+ idx)))))
       
(defun parse-suffixes* (suffixes)
  (when (and suffixes (> (length suffixes) 0))
    #+nil
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
			      (name (print (decl-body name-node)))
			      (ptr-spec (parse-pointer-spec* ptr)))
			 (list (cpp-token-value (decl-value name))
			       ptr-spec)))))
	 (suffix (parse-suffixes* (decl-body name))))
    #+nil
    (print `(://////// parse-name-spec* :@ 1
	     :name-node==> ,name-node :name-decl==> ,name-decl
	     :suffix==> ,suffix))

    (break)))

(defun parse-struct-declarator (decl)
  #+nil
  (print `(:!!!!!!!! parse-struct-declarator :@ 0 :decl==> ,decl))
  (unless (eq (decl-props decl) :c-decl-struct-declarator)
    (error "~S is not a struct declarator" decl))
  (let* ((declarator (decl-value decl))
	 (ptr-spec (parse-pointer-spec* (decl-value declarator))))
    (parse-name-spec* (decl-body declarator))
    #+nil
    (print `(:!!!!!!!! parse-struct-declarator :@ 1
	     :ptr-spec==> ,ptr-spec :name-spec==> ,(decl-body declarator)))
    #+nil
    (break)))
	
(defun allocate-named-body (names template acc &optional (idx 0))
  (if (>= idx (length names))
      acc
      (let ((name (aref names idx)))
	#+nil
	(print `(:++++++++ allocate-named-body :@ 1
		 :idx==> ,idx :name==> ,name))
	#+nil
	(break)
	(parse-struct-declarator name)
	(allocate-named-body names template acc (1+ idx)))))

(defun parse-struct/union-body (body &optional
				 (idx 0)
				 (acc (make-array 4 :adjustable t
						    :fill-pointer 0)))
  (declare (ignorable idx acc))

  (let* ((field (aref body idx))
	 (specs (decl-value field))
	 (names (decl-body field)))
    (declare (ignorable names))
    #+nil
    (print `(:@@@@@@@@ parse-struct-body :@ :specs==> ,specs :names==> ,names))
    (multiple-value-bind (ql sl)
	(loop :for elt :across (decl-value specs)
	      ;;:do (print `(:=======> ,elt))
	      :when (eq (decl-props elt) :c-decl-type-qualifier)
		:collect (decl-value elt) :into ql
	      :when (eq (decl-props elt) :c-decl-type-specifier)
		:collect (decl-value elt) :into sl
	      :finally (return (values ql sl)))
      (let* ((qual-list (make-array 4 :adjustable t :fill-pointer 0)))
	(loop :for elt :in ql
	      :do (vector-push-extend (cpp-token-props elt) qual-list))
	#+nil
	(print `(:++++++++ parse-struct-body :@ 1 :qual-list==> ,qual-list))
	(let* ((spec (car sl))
	       (template (typecase spec
			   (cpp-token
			    #+nil
			    (break)
			    (let* ((type (identify-scalar-type sl)))
			      #+nil
			      (print `(:++++++++ parse-struct-body :@ 2
				       :type==> ,type :ql==> ,ql))
			      #+nil
			      (break)
			      (mkacsf :spec type :qual qual-list)))
			   (c-declaration
			    (let ((props (decl-props spec)))
			      #+nil
			      (print `(:>>>> :props==> ,props :spec==> ,spec))
			      (unless (or (eq props :c-decl-struct-specifier)
					  (eq props :c-decl-union-specifier))
				(error "not struct specifier nor union specifier"))
			      (break))))))
	  #+nmil
	  (print `(:++++++++ parse-struct-body :@ 3 :template==> ,template))
	  #+nil
	  (break)
	  (unless (eq (decl-props names) :c-decl-struct-declarator-list)
	    (error "~S is not a :c-decl-struct-declarator" names))
	  (allocate-named-body (decl-value names) template acc)
	  #+nil
	  (break))))))

(defun parse-spec-qual-list
    (spec-list &optional
		 (idx 0)
		 (sc nil)
		 (td nil)
		 (tl (make-array 1 :adjustable t :fill-pointer 0))
		 (ql (make-array 1 :adjustable t :fill-pointer 0)))
  (declare (ignorable idx sc tl ql))
  #+nil
  (print `(:======== parse-spec-qual-list
	   :spec-list==> ,(aref spec-list idx)))
  (cond
    ((>= idx (length spec-list))
     #+nil
     (print `(:++++++++ :>>>> :sc==> ,sc :td==> ,td :tl==> ,tl :ql==> ,ql))
     (values sc td tl ql))
    (t
     #+nil
     (print `(:++++++++ :<<<< parse-spec-qual-list
			:spec-list[idx]=> ,(aref spec-list idx)))
     (let ((decl (aref spec-list idx)))
       (case (decl-props decl)
	 (:c-decl-storage-class-specifier
	  (let* ((token (decl-value decl))
		 (type (cpp-token-props token)))
	    (if (or sc td)
		(warn "mutiple storage-class specified ~S and ~S" sc type)
		(if (eq type :c-keyword-typedef)
		    (setf td t)
		    (setf sc decl)))))
	 (:c-decl-type-qualifier
	  (vector-push-extend decl ql))
	 (:c-decl-type-specifier
	  (vector-push-extend decl tl)))
       (parse-spec-qual-list spec-list (1+ idx) sc td tl ql)))))

(defun parse-name-list (name-list &optional
				    (idx 0)
				    (acc (make-array 1 :adjustable t
						       :fill-pointer 0)))
  (cond
    ((null name-list) nil)
    ((>= idx (length name-list))
     #+nil
     (print `(:++++++++ :>>>> parse-name-list :acc==> ,acc))
     acc)
    (t
     #+nil
     (print `(:++++++++ parse-name-list :<<<< :name==> ,(aref name-list idx)))
     (let ((name (aref name-list idx)))
       (unless (eq (decl-props name) :c-decl-declarator)
       (let ((decl (if (eq (decl-props name) :c-decl-init-declarator)
		       (decl-value name)
		       name)))
	 #+nil
	 (print `(:++++++++ parse-name-list :@ 1 :decl==> ,decl))
       (let ((ptr-decl (decl-value decl)))
	 #+nil
	 (print `(:++++++++ parse-name-list :@ 2 :ptr==> ,ptr-decl))
       (let ((token-wrapper (decl-body decl)))
	 #+nil
	 (print `(:++++++++ parse-name-list :@ 3 :decl==> ,token-wrapper))
	 ;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 (case (decl-props token-wrapper)
	   (:c-decl-direct-declarator
	    #+nil
	    (print `(:++++++++ parse-name-list :@ 4 :c-decl-direct-declarator))
	    (let ((body (decl-body token-wrapper)))
	      #+nil
	      (print `(:++++++++ parse-name-list :@ 5 :body==> ,body))
	    (let ((suffixes (and (> (length body) 0)
				 (parse-suffixes (decl-body token-wrapper))))
		  (token (decl-value token-wrapper)))
	      (declare (ignorable suffixes))
	      #+nil
	      (print `(:++++++++ parse-name-list :@ 6
		       :suffixes==> ,suffixes :token==> ,token))
	    (let ((name (mkacdecl :pointer (parse-pointer-spec ptr-decl)
				  :name (cpp-token-value token))))
	      #+nil
	      (print `(:++++++++ parse-name-list :@ 7 :name==> ,name))
	      (vector-push-extend (vector name suffixes) acc))))))))))
	 (parse-name-list name-list (1+ idx) acc)))))

(defun parse-name-init (name)
  )

(defun parse-suffixes (suffixes &optional
				  (acc (make-array 1 :adjustable t
						     :fill-pointer 0))
				  (idx 0))
  #+nil
  (print `(:++++++++ :<<<< parse-suffixes :idx==> ,idx))
  (when suffixes
    (cond
      ((>= idx (length suffixes))
       #+nil
       (print `(:++++++++ :>>>> parse-suffixes :acc==> ,acc))
       acc)
      (t
       #+nil
       (print `(:++++++++ :@ parse-suffixes
			  :suffixes[idx]==> ,(aref suffixes idx)))
       (let ((suffix (aref suffixes idx)))
	 (typecase suffix
	   (c-array-index
	    #+nil
	    (print `(:++++++++ :@@ parse-suffixes c-array-index))
	    (vector-push-extend (vector :c-array-index (cai-index suffix))
				acc))
	   (t
	    (let ((decls (parse-suffix (decl-value suffix))))
	      (vector-push-extend (vector (decl-props suffix) decls) acc)
	      #+nil
	      (print `(:++++++++ :!!!! :@@@ parse-suffixes
		       :decls==> ,decls))))))
       (parse-suffixes suffixes acc (1+ idx))))))

(defun parse-suffix (suffix &optional (acc (make-array 4 :adjustable t
							 :fill-pointer 0)))
  #+nil
  (print `(:++++++++ :<<<< parse-suffix
		     :type==> ,(type-of suffix) :suffix==> ,suffix))
  (when suffix
    (cond
      ((eq (decl-props suffix) :c-parameter-type-list)
       #+nil
       (print `(:++++++++ :@ parse-suffix :c-parameter-type-list))
       (parse-arg-list (decl-value suffix) acc))
      ((typep suffix 'c-array-index)
       #+nil
       (print `(:++++++++ :@ parse-suffix ,(type-of suffix)))
       (cai-index suffix)))))

(defun parse-arg-list (args &optional (acc (make-array 4 :adjustable t
							 :fill-pointer 0)))
  #+nil
  (print `(:++++++++ :<<<< parse-arg-list :args==> ,args :acc=> ,acc))
  (%parse-arg-list (decl-value args) acc))

(defun %parse-arg-list (args &optional
			     (acc (make-array 4 :adjustable t :fill-pointer 0))
			     (idx 0))
  (cond
    ((>= idx (length args))
     #+nil
     (print `(:++++++++ :>>>> %parse-arg-list :acc==> ,acc))
     acc)
    (t
     #+nil
     (print `(:++++++++ :<<<< %parse-arg-list :len==> ,(length args)
			:idx==> ,idx :args==> ,args :acc=> ,acc))
     (let* ((arg (aref args idx))
	    (arg-decl (parse-arg arg)))
       (vector-push-extend arg-decl acc)
       (%parse-arg-list args acc (1+ idx))))))

(defun parse-arg (arg)
  #+nil
  (print `(:++++++++ :<<<< parse-arg :arg==> ,arg))
  (let ((specs (decl-value arg))
	(name (decl-body arg)))
    (multiple-value-bind (tl ql) (parse-arg-spec-list specs)
      (let ((var (parse-declarator name)))
	#+nil
	(print `(:++++++++ :@@@@ parse-arg :tl==> ,tl :ql==> ,ql :var==> ,var))
	(setf (ac-var-spec var) tl
	      (ac-var-qual var) ql)
	var))))

(defun parse-arg-spec-list (decl)
  #+nil
  (print `(:++++++++ :<<<< parse-arg-spec-list :decl==> ,decl))
  (%parse-arg-spec-list (decl-value decl)))

(defun %parse-arg-spec-list (spec-list &optional
					 (tl (make-array 2 :adjustable t
							   :fill-pointer 0))
					 (ql (make-array 2 :adjustable t
							   :fill-pointer 0))
					 (idx 0))
  #+nil
  (print `(:++++++++ :<<<< %parse-arg-spec-list :spec-list==> ,spec-list))
  (cond
    ((>= idx (length spec-list)) (values tl ql))
    (t
     (let ((spec (aref spec-list idx)))
       (case (decl-props spec)
	 (:c-decl-type-qualifier
	  (let ((token (decl-value spec)))
	    (vector-push-extend (cpp-token-props token) ql)))
	 (:c-decl-type-specifier
	  (let ((obj (decl-value spec)))
	    (typecase obj
	      (cpp-token
	       (vector-push-extend (cpp-token-props obj) tl))
	      (t
	       (case (decl-props obj)
		 ((:c-decl-struct-specifier :c-decl-union-specifier)
		  (let* ((token (decl-value obj))
			 (struct (mkacstruct :type (decl-props obj)
					     :tag (cpp-token-value token))))
		    (vector-push-extend struct tl)))
		 (:c-decl-enum-specifier
		  (let* ((token (decl-value obj))
			 (enum (mkacenum :tag (cpp-token-value token))))
		    (vector-push-extend enum tl)))))))))
       (%parse-arg-spec-list spec-list tl ql (1+ idx))))))
	     
(defun parse-declarator (decl)
  #+nil
  (print `(:++++++++ :<<<< parse-declarator :decl==> ,decl))
  (let* ((ptr (parse-pointer-spec (decl-value decl)))
	 (name (decl-body decl))
	 (token (decl-value name))
	 (index (parse-index-in-param (decl-body name))))
    #+nil
    (print `(:++++++++ :@@@@ parse-declarator :ptr===> ,ptr :name==> ,name
		       :token==> ,token :index==> ,index))
    (mkacvar :name (cpp-token-value token) :ptr ptr :idx index)))

(defun parse-index-in-param (idx-list &optional
					(acc (make-array 2 :adjustable t
							   :fill-pointer 0))
					(idx 0))
  (cond
    ((>= idx (length idx-list))
     #+nil
     (print `(:++++++++ :>>>> parse-index-in-param :acc=> ,acc))
     acc)
    (t
     #+nil
     (print `(:++++++++ :<<<< parse-index-in-param
			:idx==> ,idx :idx-list==> ,(aref idx-list idx)))
     (let* ((decl (aref idx-list idx))
	    (cai (cai-index decl)))
       #+nil
       (print `(:+++++++++++++ :@@@@@@ :idx==> ,idx :cai-type==> ,(type-of cai)
		:cai==> ,cai))
       (typecase cai
	 (null
	  (vector-push-extend nil acc))
	 (c-expression
	  (vector-push-extend (cpp-eval-expr cai) acc))
	 (cpp-token
	  (vector-push-extend (cpp-token-value cai) acc))
	 (otherwise
	  (warn "unkown type of cai ~S" (type-of cai)))))
     (parse-index-in-param idx-list acc (1+ idx)))))

(defun parse-initializer-list (obj)
  (let ((init-list (decl-value obj)))
    (loop :for init :across init-list
	  :collect
	  (let* ((elt (decl-value init))
		 (val (decl-body init))
		 (indexes (make-array 16 :fill-pointer 0 :adjustable t)))
	    (loop :for idx :across (decl-value (decl-value elt))
		  :do (let ((val-type (cpp-token-value (decl-value idx)))
			    (val-token (decl-body idx)))
			(cond
			  ((string= val-type "[")
			   (vector-push-extend
 			    (cons :index
				  (cpp-token-value val-token))
			    indexes))
			  ((string= val-type ".")
			   (vector-push-extend
			    (cons :field
				  (cpp-token-value val-token))
				  indexes)))))
	    (let* ((token (decl-value val)))
		 (cons indexes (cpp-token-value token)))))))

(defun register-struct (spec)
  #+nil
  (print `(:[[[[[[[[ register-struct :spec==> ,spec))
  (let* ((name-obj (decl-value spec))
	 (name (or (and name-obj (cpp-token-value name-obj))
		   (unique-tag-name))))
    (multiple-value-bind (struct found) (gethash name *tag-hash*)
      (if (and found (ac-struct-fields struct))
	  (warn "warning: redefinition od struct ~A." name)
	  (let* ((type (decl-props spec))
		 (struct (or struct
			     (mkacstruct :type (if (eq type
						       :c-decl-struct-specifier)
						   :c-keyword-struct
						   :c-keyword-union)
					 :tag name))))
	    (parse-struct-declaration (decl-body spec) struct)
	    (setf (gethash name *tag-hash*) struct)
	    #+nil
	    (print struct)
	    (values struct type))))))

(defun parse-struct-declaration (spec struct &optional (idx 0))
  #+nil
  (print `(:-------- parse-struct-declaration ,spec))
  (cond ((>= idx (length spec)) struct)
	(t
	 (parse-struct-field-declaration (aref spec idx) struct)
	 (parse-struct-declaration spec struct (1+ idx))
	 #+nil
	 (print `(:]]]]]]]] :struct==> ,struct)))))

(defun parse-struct-field-declaration (spec struct)
  #+nil
  (print `(:-------- parse-struct-field-declaration ,spec ,struct))
  (let ((spec (decl-value spec))
	(decl (decl-body spec)))
    (multiple-value-bind (type-spec type-qual align)
	(parse-struct-field-specifiers (decl-value spec))
      #+nil
      (print `(:&&&&&&&& parse-struct-field-declaration
	       :spec==> ,type-spec :qual==> ,type-qual :align==> ,align))
      #+nil
      (print `(:&&&&&&&&
	       ,(mkacsf :spec type-spec :qual type-qual :align align)))
      (let ((fields (parse-struct-field-declarator (decl-value decl)
						   (mkacsf :spec type-spec
							   :qual type-qual
							   :align align))))
	#+nil
	(print `(:-------- parse-struct-field-declaration :fields=> ,fields))
	(when (null (ac-struct-fields struct))
	  (setf (ac-struct-fields struct) (make-array 8 :adjustable t
							:fill-pointer 0)))
	(loop :for field :across fields
	      :do (progn
		    (vector-push-extend field (ac-struct-fields struct))
		    (setf (gethash (ac-sf-name field) (ac-struct-hash struct))
			  field))))
      struct)))

(defun parse-struct-field-specifiers
    (spec &optional
	    (idx 0)
	    (type (make-array 1 :adjustable t :fill-pointer 0))
	    (qual (make-array 1 :adjustable t :fill-pointer 0))
	    (align (make-array 1 :adjustable t :fill-pointer 0)))
  #+nil
  (print `(:******** parse-struct-field-specifiers
	   :spec==> ,spec :type==> ,type :qual==> ,qual :align==> ,align))
  (cond
    ((>= idx (length spec))
     #+nil
     (print `(:======== parse-struct-field-specifiers
	      :spec==> ,type :qual==> ,qual :align==> ,align))
     (values type qual align))
    (t
     (let ((decl (aref spec idx)))
       (case (decl-props decl)
	 (:c-decl-type-specifier
	  (let ((type-spec (parse-struct-type-specifier decl)))
	    (vector-push-extend type-spec type)))
	 (:c-decl-type-qualifier
	  (let ((qual-spec (parse-struct-type-qualifier decl)))
	    (vector-push-extend qual-spec qual)))
	 (:c-decl-alignment-specifier
	  (let ((align-spec (parse-alignement decl)))
	    (vector-push-extend align-spec align))))
       (parse-struct-field-specifiers spec (1+ idx) type qual align)))))

(defun parse-struct-type-specifier (spec)
  #+nil
  (print `(:======== parse-struct-type-specifier :spec==> ,spec))
  (let ((token (decl-value spec)))
    (cpp-token-props token)))

(defun parse-struct-type-qualifier (spec)
  #+nil
  (print `(:======== parse-struct-qual-specifier :spec ,spec))
  (let ((token (decl-value spec)))
    (cpp-token-props token)))

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

(defun parse-struct-field-declarator
    (decl spec &optional (idx 0)
		 (acc (make-array 1 :adjustable t :fill-pointer 0)))
  #+nil
  (print `(:-------- parse-struct-field-declarator
	   :decl==> ,decl :spec==> ,spec))
  (cond
    ((>= idx (length decl))
     #+nil
     (print `(:^^^^^^^^ parse-struct-field-declarator :acc==> acc))
     acc)
    (t
     (let ((decl_ (aref decl idx)))
       #+nil
       (print `(:$$$$$$$$$ :decl==> ,decl_))
     (let ((name-decl (decl-value decl_)))
       #+nil
       (print `(:$$$$$$$$$ :name-decl==> ,name-decl))
     (let ((bits (decl-body decl_)))
       #+nil
       (print `(:$$$$$$$$$ :bits==> ,bits))
     (let ((ptr-spec (and name-decl (decl-value name-decl))))
       #+nil
       (print `(:$$$$$$$$$ :ptr-spec==> ,ptr-spec))
     (let ((name (and name-decl (decl-body name-decl))))
       #+nil
       (print `(:$$$$$$$$$ :name==> ,name))
     (let ((name-string (or (and name (cpp-token-value (decl-value name)))
			    (unique-field-name))))
       #+nil
       (print `(:$$$$$$$$$ :name-string==> ,name-string))
       (vector-push-extend (mkacsf :name name-string
				   :ptr-spec (parse-pointer-spec ptr-spec)
				   :bits (and bits (cpp-token-value bits))
				   :spec (ac-sf-spec spec)
				   :qual (ac-sf-qual spec)
				   :align (ac-sf-align spec))
			   acc)
       (parse-struct-field-declarator decl spec (1+ idx) acc))))))))))

(let ((unique-field-counter 0))
  (defun unique-field-name ()
    (let ((field (format nil "$!field~D!$" unique-field-counter)))
      (incf unique-field-counter)
      field)))

(defun parse-pointer-spec (decl &optional (acc (make-array 1 :adjustable t
							     :fill-pointer 0)))
  #+nil
  (print `(:-------- parse-pointer-spec :decl==> ,decl))
  (cond
    ((null decl)
     (mkacptr :qual acc))
    (t
     (let* ((qual (decl-value decl))
	    (qual (or (and qual (decl-value qual)) (make-array 0)))
	    (qual-list (make-array 1 :adjustable 5 :fill-pointer 0)))
       (loop :for elt :across qual
	     :do (let ((name (decl-value elt)))
		   (vector-push-extend (cpp-token-props name) qual-list)))
       (vector-push-extend qual-list acc)
       (parse-pointer-spec (decl-body decl) acc)))))

(defun register-enum (spec)
  #+nil
  (print `(register-enum ,spec))
  (let* ((tag-obj (decl-value spec))
	 (tag (or (and tag-obj (cpp-token-value tag-obj)) (unique-tag-name)))
	 (elt-list (decl-body spec))
	 (enum (mkacenum :tag tag)))
    (setf (gethash tag *tag-hash*) enum)
    (setf (ac-enum-items enum) (make-array 4 :adjustable t :fill-pointer 0))
    (loop :with cval := -1
	  :for elt :across elt-list
	  :do (let* ((name (cpp-token-value (decl-value elt)))
		     (expr (decl-body elt))
		     (value (and expr (cpp-eval-expr expr))))
		(if value
		    (setf cval value)
		    (setf value (incf cval)))
		(let ((entry (mkacenume :name name :value value)))
		  #+nil
		  (print `("+++++++++++++" ,value ,cval))
		  (vector-push-extend entry (ac-enum-items enum))
		  (setf (gethash name (ac-enum-hash enum)) entry))))
    #+nil
    (print enum)
    enum))

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
