(in-package :c-anl)

#|
…or create a new repository on the command line

echo "# pars-comb-test" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M master
git remote add origin git@github.com:kom-xxx/pars-comb-test.git
git push -u origin master

…or push an existing repository from the command line

git remote add origin git@github.com:kom-xxx/pars-comb-test.git
git branch -M master
git push -u origin master

…or import code from another repository

You can initialize this repository with code from a Subversion, Mercurial, or TFS project.
|#

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
(defvar *typdef-hash* (make-hash-table :test #'equal))
(defvar *name-hash* (make-hash-table :test #'equal))

(defvar *token-input* nil)
(defvar *parsed-tree* nil)
(defvar *rest-tokens* nil)

(defun preprocess (file &optional (builtin t))
  (setf cpp-directive::*cpp-macro-hash* (make-hash-table :test #'equal))
  (setf c-stmt::*typedef-hash* (make-hash-table :test #'equal))
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
  (format t "~A~S~%" (subseq *indent* 1) obj))

(defun analyse (tree)
  (declare (type (or c-declaration c-statement c-expression)))
  (typecase tree
    (c-statement
     (analyse-stmt tree))
    (c-declaration
     (analyse-decl tree))
    (c-expression
     (analyse-expr tree))))

(defun analyse-stmt (tree)
  (let ((props (stmt-props tree)))
    (case props
      (:c-stmt-translation-unit
       (loop :for tree :across (stmt-value tree)
	     :do (analyse tree))))))

(defun analyse-decl (tree)
  (let ((props (decl-props tree)))
    (case props
      (:c-declaration
       (process-declaration tree)))))

(defun analyse-expr (tree)
  (declare (ignorable tree)))

(defstruct (c-variable (:conc-name c-var-) (:constructor mkcvar))
  (name "" :type string)
  (scs nil :type (or array null))
  (type nil :type (or array null))
  (typeq nil :type (or array nil))
  (idx nil :type (or array null))
  (init nil :type t))

(defun process-declaration (tree)
  (print `(process-declaration ,tree))
  (let ((specs (decl-value tree))
	(names (decl-body tree))
	(storage-class nil)
	(typeq-list (make-array 8 :fill-pointer 0 :adjustable t))
	(types-list (make-array 8 :fill-pointer 0 :adjustable t))
	(name-list (make-array 8 :fill-pointer 0 :adjustable t)))
    (when specs
      (print (decl-props specs))
      (loop :for spec :across (decl-value specs)
	    :do (let ((prop (decl-props spec)))
		  (declare (type keyword prop))
		  (print `(:@@@@> ,prop))
		  (case prop
		    (:c-decl-storage-class-specifier
		     (let ((token (decl-value spec)))
		       (format t "~%  ~S ~S"
			       (cpp-token-props token) (cpp-token-value token))
		       (if (null storage-class)
			   (setf storage-class (cpp-token-props token))
			   (error "cannot combine multiple storage-class."))))
		    (:c-decl-type-qualifier
		     (let ((token (decl-value spec)))
		       (format t "~%  ~S ~S"
			       (cpp-token-props token) (cpp-token-value token))
		       (vector-push-extend (cpp-token-props token) typeq-list)))
		    (:c-decl-type-specifier
		     (if (typep (decl-value spec) 'cpp-token)
			 (let ((token (decl-value spec)))
			   (format t "~%  ~S ~S"
				   (cpp-token-props token)
				   (cpp-token-value token))
			   (vector-push-extend (list (cpp-token-props token)
						     token)
					       types-list))
			 (let ((spec (decl-value spec)))
			   (case (decl-props spec)
			     ((:c-decl-struct-specifier
			       :c-decl-union-specifier)
			      (vector-push-extend (list (decl-props spec)
							(register-struct spec))
						  types-list))
			     (:c-decl-enum-specifier
			      (vector-push-extend (list (decl-props spec)
							(register-enum spec))
						  types-list))
			     (:c-decl-atomic-type-specifier
			      (vector-push-extend (list (decl-props spec)
							(atomic-type spec))
						  types-list))))))))))
    (when names
      (print `(:======== ,(decl-props names)))
      (loop :for name :across (decl-value names)
	    :do (let* ((decl (decl-value name))
		       (init (decl-body name))
		       (name-obj (decl-body decl))
		       (name-props (decl-props decl))
		       (name-token (decl-value name-obj))
		       (name-body (decl-body name-obj))
		       (indexes nil))
		  (print `(:>>>>>>>>> :decl==> ,decl :init==> ,init
				      :name-obj==> ,name-obj
				      :name-props ,name-props
				      :name-token==> ,name-token
				      :name-body==> ,name-body))
		  (when (/= (length name-body) 0)
		    (setf indexes (make-array 16 :fill-pointer 0 :adjustable t))
		    (loop :for idx :across name-body
			  :do (let ((idx (cai-index idx)))
				(vector-push-extend idx indexes))))

		  (let* ((init-obj
			   (if (and init
				    (eq (decl-props init) :c-decl-initializer))
			       (decl-value init)
			       nil))
			 (init-val
			   (progn
			     (typecase init-obj
			       (cpp-token
				(cpp-token-value init-obj))
			       (c-declaration
				(case (decl-props init-obj)
				  (:c-decl-initializer-list
				   (parse-initializer-list init-obj))))
			       (t nil)))))
		    (vector-push-extend
		     (mkcvar :name (cpp-token-value name-token)
			     :type types-list :typeq typeq-list
			     :init init-val
			     :idx indexes)
		     name-list)
		    (setf (gethash (cpp-token-value name-token) *name-hash*)
			  (cpp-token-value name-token))
		    (print `(:====> ,(tail name-list)))))))))

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
  (print `(register-struct ,spec))
  (let ((*name-hash* (make-hash-table :test #'equal))
	(type (decl-props spec))
	(name (or (decl-value spec) (unique-tag-name))))
    (declare (ignorable type name))))

(defvar *unique-tag-counter* 0)
(defun unique-tag-name ()
  (let ((tag (format nil "@struct/union/enum-~D" *unique-tag-counter*)))
    (incf *unique-tag-counter*)
    tag))

(defun register-enum (spec)
  (print `(register-enum ,spec)))

(defun atomic-type (spec)
  (print `(atomic-type ,spec)))
