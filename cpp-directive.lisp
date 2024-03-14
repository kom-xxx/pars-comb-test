;;;;
#|
!!! TODO !!!
1. need __VS_ARGS__ expander
|#

(in-package :cpp-directive)

(defvar *cpp-macro-hash* (make-hash-table :test #'equal))
(defparameter *cpp-system-include-path*
  #+linux'("/usr/lib/gcc/x86_64-linux-gnu/11/include" "/usr/local/include"
	   "/usr/include/x86_64-linux-gnu" "/usr/include")
  #+freebsd'("/usr/lib/clang/17/include" "/usr/include"))
(defparameter *cpp-user-include-path* ())

(defvar *debug* nil)

(defun dprint (x)
  (when *debug*
    (print x)))

(defun dump (acc &optional (label nil))
  (when *debug*
    (loop :for token :across acc
	  :collect (cpp-token-value token) :into list
	  :finally #+nil
		   (print (if label `(,label ,@list) list))
		   (format t "#(~%~A ~{~S~^ ~})" label list ))))

(defun stringify (str)
  (let ((acc (makeq 'character)))
    (loop :for c :across str
	  :when (or (char= c #\") (char= c #\\))
	    :do (vector-push-extend #\\ acc)
	  :end
	  :do (vector-push-extend c acc))
    acc))

(defun make-file-input-state (fname)
  #-nil
  (print `(make-file-input-state ,fname))
  (with-open-file (is fname :direction :input
			    :if-does-not-exist :error
			    :element-type '(unsigned-byte 8))
    (let* ((buf-len (file-length is))
	   (buf (make-array buf-len :element-type '(unsigned-byte 8))))
      (read-sequence buf is)
      (mkcis :i (sb-ext:octets-to-string buf) :f fname))))

(defun /token (state parser &optional (acc (makeq 'cpp-token)))
  (declare (type char-input-state state)
	   (type (simple-parser char-input-state (queue cpp-token)) parser))
  #+nil
  (print `(/token ,(type-of acc)
		  ,(typep acc '(array cpp-token 1))
		  ,(typep acc '(vector cpp-token))))
  (.do ((space (/whitespace* state (makeq 'cpp-token)))
	(queue  (funcall parser nstate acc)))
    (let ((token (tail queue)))
      (if token
	  (let ((have-space (loop :for token :across space
				  :when (/= (length (cpp-token-value token)) 0)
				    :return t
				  :finally (return nil))))
	    (setf (cpp-token-spaces token) have-space)
	    (values queue nstate))
	  (values nil nil)))))

#+debug-/many
(defvar */many-force-print* nil)
(defun /many (state parser &optional (acc (makeq 'token))
                                     (min 0)
		                     (max #.(1- array-dimension-limit))
                                     (cnt 0))
  #+debug-/many
  (when */many-force-print*
    (print `(****** /many state-offset=> ,(is-o state))))
  (multiple-value-bind (result nstate) (funcall parser state acc)
    (if (or (>= cnt max) (null result) (eq state nstate))
        (if (< cnt min)
            (values nil nil)
            (values acc state))
        (/many nstate parser result min max (1+ cnt)))))

(defun /many*
    (state parser &optional (acc (makeq 'cpp-token)) &rest parser-args)
  (multiple-value-bind (result nstate) (apply parser state acc parser-args)
    (if (null result)
	(values acc state)
	(apply #'/many* nstate parser acc parser-args))))

(defmacro !token (parser)
  `#'(lambda (state acc)
       (multiple-value-bind (result nstate) (/token state #',parser acc)
	 (values result nstate))))

(defun =cpp-file (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-file)
  (.any (.do ((group (=cpp-group state acc)))
	  #+nil
	  (print `(<<< =cpp-file ,group ,nstate))
	  (values group nstate))
	(progn
	  #+nil
	  (print `(<<< =cpp-file ,acc ,state))
	  (values acc state))))

(defun =cpp-group (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-group)
  (.do ((group-part (/many state #'=cpp-group-part (makeq 'cpp-directive) 1)))
    (let ((directive (make-cpp-directive :props :cpp-group :value group-part)))
      (values (enq directive acc) nstate))))


(defun =cpp-group-part (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-group-part)
  (.any (=cpp-if-section state acc)
	(=cpp-control-line state acc)
	(=cpp-non-directive state acc)
	(=cpp-text-line state acc)))

(defun =cpp-if-section (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-if-section)
  (.do ((if-group (=cpp-if-group state (makeq 'cpp-directive)))
	(elif-group (/many nstate #'=cpp-elif-group if-group 0))
	(else-group (/many nstate #'=cpp-else-group elif-group 0 1))
	(endif-line (=cpp-endif-line nstate else-group)))
    (let ((directive (make-cpp-directive :props :cpp-if-section
					 :value endif-line)))
      (values (enq directive acc) nstate))))

(defstruct (cpp-conditional (:include cpp-directive) (:conc-name cpp-cond-))
  (test nil :type (or (queue cpp-token) null)))

(defun =cpp-if-directive (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-if-directive)
  (.do ((hash (/token state #'/hash (makeq 'cpp-token))))
    (.any (.do ((ifdef (/token nstate #'/cpp-ifdef))
		(identifier (/token nstate #'/identifier))
		(newline (/token nstate #'/newline)))
	    (let ((directive (make-cpp-conditional :props :cpp-ifdef
						   :test identifier)))
	      (values (enq directive acc) nstate)))
	  (.do ((ifndef (/token nstate #'/cpp-ifndef))
		(identifier (/token nstate #'/identifier))
		(newline (/token nstate #'/newline)))
	    (let ((directive (make-cpp-conditional :props :cpp-ifndef
						   :test identifier)))
	      (values (enq directive acc) nstate)))
	  (.do ((if (/token nstate #'/cpp-if))
		(expression (/many nstate (!token /pp-token)))
		(newline (/token nstate #'/newline)))
	    #+nil
	    (print `(=cpp-if-directive ,expression))
	    (let ((directive (make-cpp-conditional :props :cpp-if
						   :test expression)))
	      (values (enq directive acc) nstate))))))

(defun =cpp-if-group (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-if-group)
  (.do ((if-directive (=cpp-if-directive state acc)))
    (.any (.do ((group (=cpp-group nstate)))
	    (let ((directive (tail if-directive)))
	      (if (null directive)
		  (values nil nil)
		  (progn
		    (setf (cpp-cond-value directive) group)
		    (values if-directive nstate)))))
	  (values if-directive nstate))))

(defun =cpp-elif-directive (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-elif-directive)
  (.do ((hash (/token state #'/hash))
	(elif (/token nstate #'/cpp-elif))
	(expression (/many nstate (!token /pp-token)))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-conditional :props :cpp-elif
					   :test expression)))
      (values (enq directive acc) nstate))))

(defun =cpp-elif-group (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-elif-group)
  (.do ((elif-directive (=cpp-elif-directive state acc)))
    (.any (.do ((group (=cpp-group nstate)))
	    (let ((directive (tail elif-directive)))
	      (if (null directive)
		  (values nil nil)
		  (progn
		    (setf (cpp-cond-value directive) group)
		    (values elif-directive nstate)))))
	  (values elif-directive nstate))))

(defun =cpp-else-directive (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-else-directive)
  (.do ((hash (/token state #'/hash))
	(else (/token nstate #'/cpp-else))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-conditional :props :cpp-else)))
      (values (enq directive acc) nstate))))

(defun =cpp-else-group (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-else-group)
  (.do ((else-directive (=cpp-else-directive state acc)))
    (.any (.do ((group (=cpp-group nstate)))
	    (let ((directive (tail else-directive)))
	      (if (null directive)
		  (values nil nil)
		  (progn
		    (setf (cpp-cond-value directive) group)
		    (values else-directive nstate)))))
	  (values else-directive nstate))))

(defun =cpp-endif-line (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-endif-line)
  (.do ((hash (/token state #'/hash))
	(endif (/token nstate #'/cpp-endif))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-directive :props :cpp-endif)))
      (values (enq directive acc) nstate))))

(defun =cpp-control-line (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-control-line)
  (.any (=cpp-include state acc)
	(=cpp-define state acc)
	(=cpp-undef state acc)
	(=cpp-line state acc)
	(=cpp-error state acc)
	(=cpp-pragma state acc)
	(=cpp-empty state acc)))

(defun =cpp-include (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print `(=cpp-include ,state ,acc))
  (.do ((hash (/token state #'/hash))
	(include (/token nstate #'/cpp-include))
	(tokens (/many nstate (!token /pp-token) (makeq 'cpp-token) 1))
	(newline (/token nstate #'/newline)))
    #+nil
    (print `(=cpp-include ,hash ,include ,tokens ,newline))
    (let ((directive (make-cpp-directive :props :cpp-include
					 :value tokens)))
      (enq directive acc)
      #+nil
      (print `(@@@ =cpp-include ,acc #+nil ,nstate))
      (values acc nstate))))

(defstruct (cpp-define (:include cpp-directive) (:conc-name cpp-def-))
  (replace nil :type (or (queue cpp-token) null))
  (params nil :type (or (queue cpp-token) null))
  (param-hash nil :type (or hash-table null))
  (three-dots nil :type boolean)
  (expanding nil :type boolean)
  (current-args nil :type (or (vector cpp-token *) null)))

(defun /id-list-rest (state &optional (acc (makeq 'cpp-token)))
  (.do ((comma (/token state #'/comma))
	(id (/token nstate #'/identifier acc)))
    (values id nstate)))

(defun make-param-hash (param-vec)
  (let ((param-hash (make-hash-table :test #'equal)))
    (loop :for param :across param-vec
	  :for i :from 0
	  :do (let ((key (cpp-token-value param)))
		(multiple-value-bind (token found) (gethash key param-hash)
		  (declare (ignore token))
		  (if found
		      (error "duplicate macro parameter ~S" key)
		      (progn
			(setf (gethash key param-hash) i))))))
    param-hash))
		  
(defun =cpp-define (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-define)
  (.do ((hash (/token state #'/hash))
	(define (/token nstate #'/cpp-define))
	(macro-name (/token nstate #'/identifier)))
    (.any
     (progn
       #+nil
       (print `(cpp-define ,macro-name newline))
       (.do ((newline (/token nstate #'/newline)))
	 (let ((directive (make-cpp-define :props :cpp-define
					   :value macro-name
					   :param-hash (make-hash-table :test #'equal))))
	   (values (enq directive acc) nstate))))
     (progn
       #+nil
       (print `(=cpp-define function like macro))
       (.do ((lparen (/lparen nstate)))
	 (.any (.do ((id (/token nstate #'/identifier))
		     (rest (/many nstate #'/id-list-rest id))
		     (comma (/token nstate #'/comma))
		     (dots (/token nstate #'/three-dots))
		     (rparen (/token nstate #'/rparen))
		     (replace (/many nstate (!token /pp-token)))
		     (newline (/token nstate #'/newline)))
		 (let ((directive (make-cpp-define :props :cpp-define
						   :value macro-name
						   :params rest
						   :param-hash
						   (make-param-hash rest)
						   :three-dots t
						   :replace replace)))
		   (values (enq directive acc) nstate)))
	       (.do ((id (/token nstate #'/identifier))
		     (rest (/many nstate #'/id-list-rest id))
		     (rparen (/token nstate #'/rparen))
		     (replace (/many nstate (!token /pp-token)))
		     (newline (/token nstate #'/newline)))
		 (let ((directive (make-cpp-define :props :cpp-define
						   :value macro-name
						   :params rest
						   :param-hash
						   (make-param-hash rest)
						   :replace replace)))
		   (values (enq directive acc) nstate)))
	       (.do ((dots (/token nstate #'/three-dots))
		     (rparen (/token nstate #'/rparen))
		     (replace (/many nstate (!token /pp-token)))
		     (newline (/token nstate #'/newline)))
		 (let ((directive (make-cpp-define :props :cpp-define
						   :value macro-name
						   :three-dots t
						   :replace replace)))
		   (values (enq directive acc) nstate)))
	       (.do ((rparen (/token nstate #'/rparen))
		     (replace (/many nstate (!token /pp-token)))
		     (newline (/token nstate #'/newline)))
		 (let ((directive (make-cpp-define :props :cpp-define
						   :value macro-name
						   :replace replace)))
		   (values (enq directive acc) nstate))))))
     (progn
       #+nil
       (print `(=cpp-define ,macro-name replace newline))
       (.do ((space (/whitespace* nstate (makeq 'cpp-token) 1))
	     (replace (/many nstate (!token /pp-token)))
	     (newline (/token nstate #'/newline)))
	 (let ((directive (make-cpp-define :props :cpp-define
					   :value macro-name
					   :replace replace
					   :param-hash (make-hash-table :test #'equal))))
	   (values (enq directive acc) nstate)))))))

(defun =cpp-undef (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-undef)
  (.do ((hash (/token state #'/hash))
	(undef (/token nstate #'/cpp-undef))
	(macro-name (/token nstate #'/identifier))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-define :props :cpp-undef
				      :value macro-name)))
      (values (enq directive acc) nstate))))

(defun =cpp-line (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-line)
  (.do ((hash (/token state #'/hash))
	(line (/token nstate #'/cpp-line))
	(rest (/many nstate (!token /pp-token) (makeq 'cpp-token) 1))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-directive :props :cpp-line
					 :value rest)))
      (values (enq directive acc) nstate))))

(defun =cpp-error (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-error)
  (.do ((hash (/token state #'/hash))
	(error (/token nstate #'/cpp-error))
	(message (/many nstate (!token /pp-token)))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-directive :props :cpp-error
					 :value message)))
      (values (enq directive acc) nstate))))
  
(defun =cpp-pragma (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-pragma)
  (.do ((hash (/token state #'/hash))
	(pragma (/token nstate #'/cpp-pragma))
	(message (/many nstate (!token /pp-token)))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-directive :props :cpp-pragma
					 :value message)))
      (values (enq directive acc) nstate))))

(defun =cpp-empty (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-empty)
  (.do ((hash (/token state #'/hash))
	(newline (/token nstate #'/newline)))
    (let ((directive (make-cpp-directive :props :cpp-empty)))
      (values (enq directive acc) nstate))))
  
(defun =cpp-non-directive (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-non-directive)
  (.do ((hash (/token state #'/hash (makeq 'cpp-token))))
    (.any (.do ((directive
		 (/token nstate (lambda (state acc)
				  (.any (/cpp-ifdef state acc)
					(/cpp-ifndef state acc)
					(/cpp-if state acc)
					(/cpp-elif state acc)
					(/cpp-else state acc)
					(/cpp-endif state acc)
					(/cpp-define state acc)
					(/cpp-undef state acc)
					(/cpp-line state acc)
					(/cpp-error state acc)
					(/cpp-pragma state acc))))
		 (declare (ignore nstate))))
	    (return-from =cpp-non-directive (values nil nil)))
	  (.do ((tokens (/many nstate (!token /pp-token) hash))
		(newline (/token nstate #'/newline tokens)))
	    (let ((directive (make-cpp-directive :props :cpp-non-directive
						 :value newline)))
	      (values (enq directive acc) nstate))))))

(defun =cpp-text-line (state &optional (acc (makeq 'cpp-directive)))
  #+nil
  (print '=cpp-text-line)
  (.any (.do ((hash (/token state #'/hash)
		    (declare (ignore nstate))))
	  (return-from =cpp-text-line (values nil niL)))
	(let* ((last (tail acc))
	       (last-props (if last
			       (eq (cpp-directive-props last) :cpp-text-line)
			       nil))
	       (token-acc (if last-props
			      (cpp-directive-value last)
			      (makeq 'cpp-token))))
	  (.any (.do ((text (/many state (!token /pp-token) token-acc))
		      (new-line (/token nstate #'/newline)))
		  (if last-props
		      (values acc nstate)
		      (let ((directive (make-cpp-directive :props :cpp-text-line
							   :value text)))
			(values (enq directive acc) nstate))))
		(.do ((newline (/token state #'/newline)))
		  (if last-props
		      (values acc nstate)
		      (let ((directive
			      (make-cpp-directive :props :cpp-text-line
						  :value (makeq 'cpp-token))))
			(values (enq directive acc) nstate))))))))

(defun ==directive-input (state)
  (declare (type input-state state))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((i (is-i state))
	 (o (is-o state))
	 (l (length i)))
    (declare (type (simple-array cpp-directive (*)) i))
    (if (< o l)
      (values (aref i o) (mkis :i i :o (1+ o)))
      (values nil nil))))

(defun =/parse-cpp-directive (state &optional (acc (makeq 'cpp-token)))
  (declare (type input-state state)
	   (type (queue cpp-token) acc))
  #+nil
  (print `(=/parse-cpp-directive ,state))
  (dump acc '=/parse-cpp-directive)
  (multiple-value-bind (directive nstate) (==directive-input state)
    #+nil
    (when directive
      (print `(@@@ =/parse-cpp-directive ,directive
					 ,(cpp-directive-props directive))))
    (cond
      ((null directive)
       (values acc nstate))
      (t
       (case (cpp-directive-props directive)
	 (:cpp-group
	  (=/cpp-group directive acc))
	 (:cpp-if-section
	  (=/cpp-if-section directive acc))
	 (:cpp-include
	  (=/cpp-include directive acc))
	 (:cpp-define
	  (=/cpp-define directive acc))
	 (:cpp-undef
	  (=/cpp-undef directive acc))
	 (:cpp-line
	  (=/cpp-line directive acc))
	 (:cpp-error
	  (=/cpp-error directive acc))
	 (:cpp-pragma
	  (=/cpp-pragma directive acc))
	 (:cpp-empty
	  (=/cpp-empty directive acc))
	 (:cpp-non-directive
	  (=/cpp-nondirective directive acc))
	 (:cpp-text-line
	  (=/cpp-text-line directive acc)))
       (=/parse-cpp-directive nstate acc)))))

(defun =/cpp-group (directive acc)
  #+nil
  (print '=/cpp-group)
  (let ((dstate (mkdis :i (cpp-directive-value directive))))
    (=/parse-cpp-directive dstate acc)))

(defun cpp-macro-defined (token)
  (let ((name (cpp-token-value token)))
    (multiple-value-bind (_ found) (gethash name *cpp-macro-hash*)
      (declare (ignore _))
      found)))

#+debug-=/cpp-conditional
(progn
  (defvar *cpp-conditional-test* nil)
  (defvar *cpp-conditional-expr* nil))

(defun =/cpp-conditional (state acc)
  #+nil
  (print `(=/cpp-conditional state=> ,state acc=> ,acc))
  (multiple-value-bind (conditional nstate) (==directive-input state)
    (if (null conditional)
	(values acc state)
	(case (cpp-directive-props conditional)
	  ((:cpp-if :cpp-elif)
	   (let ((tstate (mktis :i (cpp-cond-test conditional))))
	     (.do (((test _) (//cpp-parse-line tstate)
		    (declare (ignorable _)))
		   ((expr _)
		    (let ((tis (mktis :i test)))
		      #+debug-=/cpp-conditional
		      (setf *cpp-conditional-test* test)

		      (multiple-value-bind (expr _)
			  (/$constant-expression tis)
			#+debug-=/cpp-conditional
			(setf *cpp-conditional-expr* expr)
			(values expr _)))
		    (declare (ignore _))))
	       (let ((result (cpp-eval-expr expr)))
		 (if (/= result 0)
		     (let ((dstate (mktis :i (cpp-cond-value conditional))))
		       (.do ((body (=/parse-cpp-directive dstate acc)))
			 (values acc nstate)))
		     (=/cpp-conditional nstate acc))))))
	  (:cpp-ifdef
	   (if (cpp-macro-defined (aref (cpp-cond-test conditional) 0))
	       (let ((dstate (mkdis :i (cpp-cond-value conditional))))
		 (.do ((body (=/parse-cpp-directive dstate acc)))
		   (values body nstate)))
	       (=/cpp-conditional nstate acc)))
	  (:cpp-ifndef
	   (if (not (cpp-macro-defined (aref (cpp-cond-test conditional) 0)))
	       (let ((dstate (mkdis :i (cpp-cond-value conditional))))
		 (=/parse-cpp-directive dstate acc))
	       (=/cpp-conditional nstate acc)))
	  (:cpp-else
	   (let ((dstate (mkdis :i (cpp-cond-value conditional))))
	     (.do ((body (=/parse-cpp-directive dstate acc)))
	       (values body nstate))))
	  (:cpp-endif
	   (values acc nstate))))))
	  
(defun =/cpp-if-section (directive acc)
  #+nil
  (print '=/cpp-if-section)
  (let ((dstate (mkdis :i (cpp-directive-value directive))))
    (=/cpp-conditional dstate acc)))

(defun cpp-resolve-header-name (name)
  #+nil
  (print `(cpp-resolve-header-name ,name))
  (let* ((hdr-name (apply #'concatenate 'string
			  (loop :for token :across name
				:collect (cpp-token-value token))))
	 (type (char hdr-name 0))
	 (hdr-name (string-trim '(#\< #\> #\") hdr-name))
	 (path (loop :for path :in *cpp-system-include-path*
		     :when (open (concatenate 'string path "/" hdr-name)
				 :direction :probe)
		       :return path)))
    #+nil
    (print `(cpp-resolve-header-name ,hdr-name ,path))
    (if (and (char= type #\") (not path))
	(concatenate 'string "./" hdr-name)
	(concatenate 'string path "/" hdr-name))))

(defvar *cpp-include-directive* nil)
(defun =/cpp-include (directive acc)
  #+nil
  (print `(=/cpp-include ,directive ,acc))
  (setf *cpp-include-directive* directive)
  (let* ((token-state (mktis :i (cpp-directive-value directive)))
	 (header-name (//cpp-parse-line token-state))
	 (header (cpp-resolve-header-name header-name))
	 (input (make-file-input-state header))
	 (dstate (mkdis :i (=cpp-file input))))
    #+nil
    (print `(=/cpp-include ,token-state .header-name ,input ,dstate))
    (=/parse-cpp-directive dstate acc)
    #+nil
    (print `(=/cpp-include-acc ,acc))
    (values acc nil)))

(defun cpp-macro-diff (replace1 replace2)
  (if (/= (length replace1) (length replace2))
      t
      (loop :for elt1 :across replace1
	    :for elt2 :across replace2
	     :count (string/= (cpp-token-value elt1) (cpp-token-value elt2))
	     :into counter
	     :finally (return (/= counter 0)))))

(defun =/cpp-define (directive acc)
  (declare (ignore acc))
  #+nil
  (print '=/cpp-define)
  #+nil
  (print `(=/cpp-define ,directive))
  (let ((name (cpp-token-value (aref (cpp-def-value directive) 0))))
    #+nil
    (print `(=/cpp-define ,name))
    (multiple-value-bind (pred found) (gethash name *cpp-macro-hash*)
      #+nil
      (print `(=/cpp-define ,pred))
      (when (and found (cpp-macro-diff (cpp-def-replace directive)
				       (cpp-def-replace pred)))
	(warn "warning: ~S redifined" name))
      #+nil
      (print `(@@@ =/cpp-define--- ,name ,directive))
      (setf (gethash name *cpp-macro-hash*) directive))))

(defun =/cpp-undef (directive acc)
  (declare (ignore acc))
  (let ((name (cpp-token-value (aref (cpp-def-value directive) 0))))
    (remhash name *cpp-macro-hash*)))

(defun =/cpp-line (directive acc)
  (declare (ignore directive acc)))

(defun =/cpp-error (directive acc)
  (declare (ignore directive acc))
  (error "met #error directive"))

(defun =/cpp-pragma (directive acc)
  (declare (ignore directive acc)))

(defun =/cpp-empty (directive acc)
  (declare (ignore directive acc)))

(defun =/cpp-nondirective (directive acc)
  (declare (ignore directive acc)))

(defun =/cpp-text-line (directive acc)
  #+nil
  (print `(=/cpp-text-line ,(cpp-directive-value directive)))
  (let* ((token-state (mktis :i (cpp-directive-value directive)))
	 (expand (//cpp-parse-line token-state)))
    (loop :for token :across expand
	  :do (enq token acc))
    acc))

(defun //expand-fn-macro
    (state macro args &optional (acc (makeq 'cpp-token)))
  (declare (ignore state))
  #+nil
  (print `(//expand-fn-macro :macro=> ,macro :args=> ,args :acc=> ,acc))
  (let ((replace (makeq 'cpp-token))
	(hash (cpp-def-param-hash macro))
	(n-args (length (cpp-def-params macro))))
    (loop :for token :across (cpp-def-replace macro)
	  :do (let ((val (cpp-token-value token)))
		(multiple-value-bind (idx found) (gethash val hash)
		  (if found
		      (loop :for token :across (aref args idx)
			    :do (vector-push-extend token replace))
		      (vector-push-extend token replace)))))
    (//cpp-parse-line (mktis :i replace) acc n-args args)))

(defun //macro (state &optional (acc (makeq 'cpp-token)))
  #+nil
  (print `(//macro :state=> ,state :acc=> ,acc))
  (.any (.do (((macro xstate) (/=macro? state))
	      (args (//macro-arg xstate)))
	  (if (cpp-def-expanding macro)
	      (values (cpp-def-value macro) xstate)
	      (progn
		(setf (cpp-def-expanding macro) t)
		(//expand-fn-macro nstate macro args acc)
		(setf (cpp-def-expanding macro) nil)
		(values acc nstate))))
	(.do ((macro (/=macro? state)))
	  (if (cpp-def-expanding macro)
	      (values (cpp-def-value macro) nstate)
	      (let ((replace (cpp-def-replace macro)))
		(setf (cpp-def-expanding macro) t)
		(//cpp-parse-line (mktis :i (if replace replace #())) acc)
		(setf (cpp-def-expanding macro) nil)
		#+nil
		(print `(******** //macro :acc=> ,acc :nstate=> ,nstate))
		(values acc nstate))))))

(defun /=macro? (state)
  (.do ((id (//identifier state)))
    (multiple-value-bind (macro found)
	(gethash (token-value id) *cpp-macro-hash*)
      (if found
	  (values macro nstate)
	  (values nil nil)))))

(defun //macro-arg (state)
  (let ((acc (makeq '(queue (queue cpp-token)))))
    (enq (makeq 'cpp-token) acc)
    #+nil
    (setf */many-force-print* t)
    (.do ((lparen (//lparen state))
	  (args (//macro-arg-list nstate acc))
	  (rparen (//rparen nstate)))
      #+nil
      (print `(@@@@ //macro-arg :args=> args))
      #+nil
      (setf */many-force-print* nil)
      (values args nstate))))

(defun //macro-arg-list (state acc)
  #+nil
  (print `(//macro-arg-list :state=> ,state :acc=> ,acc))
  (/many state #'//macro-arg-elt acc))

(defun //macro-arg-elt (state &optional (acc (makeq '(queue cpp-token))))
  (declare (type (queue (queue cpp-token)) acc))

  #+nil
  (print `(//macro-arg-elt entry :state=> ,state :acc=> ,acc))
  (let ((xacc (tail acc)))
    (declare (type (queue cpp-token) xacc))

    (.any (.do ((lparen (//lparen state))
		(arg-list (progn
			    (enq lparen xacc)
			    (//macro-arg-list nstate acc)))
		(rparen (//rparen nstate)))
	    (enq rparen xacc)
	    (values acc nstate))
	  (.do ((comma (//comma state)))
	    (setf xacc (makeq 'cpp-token))
	    (enq xacc acc)
	    (values acc nstate))
	  (.do ((token (token-input state)))
	    (if (eq (cpp-token-props token) :c-punctuator-rparen)
		(values acc state)
		(progn
		  (enq token xacc)
		  (values acc nstate)))))))

(defun //cpp-line-elt (state acc &optional (n-args 0) (args nil))
  #+nil
  (print `(//cpp-line-elt :state=> ,state :acc=> ,acc :n-args=> ,n-args
			  :args=> ,args))
  (.any (//macro state acc)
	(.do ((token (token-input state))
	      (hash-hash (//hash-hash nstate))
	      (ntoken (token-input nstate)))
	  (setf (cpp-token-value token)
		(concatenate 'string (cpp-token-value token)
			     (cpp-token-value ntoken)))
	  (values (enq token acc) nstate))
	(.do ((hash (//hash state))
	      (token (token-input nstate)))
	  (if (string= (cpp-token-value token) "__VA_ARGS__")
	      (if (> n-args (length args))
		  (let ((args
			  (loop :for i :from n-args :below (length args)
				:for arg := (aref args i)
				  :then (aref args i)
				:nconc (loop :for str :across arg
					     :nconc
					     (if (cpp-token-spaces token)
						 `(" "
						   ,(cpp-token-value token))
						 `(,(cpp-token-value token))))
				  :into acc
				:finally (apply #'concatenate 'string acc))))
		    (values (enq (make-cpp-token :props :string-literal
						 :value args
						 :attr (cpp-token-attr token)
						 :state (cpp-token-state token)
						 :next (cpp-token-next token)
						 :file (cpp-token-file token)
						 :line (cpp-token-line token)
						 :colmn (cpp-token-colmn token))
				 acc)
			    nstate))
		  (values (enq (make-cpp-token :props :string-literal
					       :value ""
					       :attr (cpp-token-attr token)
					       :state (cpp-token-state token)
					       :next (cpp-token-next token)
					       :file (cpp-token-file token)
					       :line (cpp-token-line token)
					       :colmn (cpp-token-colmn token))
				 acc)
			    nstate))
	      (values (enq (make-cpp-token :props :string-literal
					   :value (cpp-token-value token)
					   :attr (cpp-token-attr token)
					   :state (cpp-token-state token)
					   :next (cpp-token-next token)
					   :file (cpp-token-file token)
					   :line (cpp-token-line token)
					   :colmn (cpp-token-colmn token))
			   acc)
		      nstate)))
	(.do ((defined (//identifier state))
	      (lparen (//lparen nstate))
	      (name (//identifier nstate))
	      (rparen (//rparen nstate)))
	  (if (string= (cpp-token-value defined) "defined")
	      (progn
		(enq (make-cpp-token :props :pp-number
				     :value (if (cpp-macro-defined name) "1" "0")
				     :state (cpp-token-state name)
				     :next (cpp-token-next name)
				     :file (cpp-token-file name)
				     :line (cpp-token-line name)
				     :colmn (cpp-token-colmn name))
		     acc)
		(values acc nstate))
	      (values acc nstate)))
	(.do ((defined (//identifier state))
	      (name (//identifier nstate)))
	  (if (string= (cpp-token-value defined) "defined")
	      (values (enq (make-cpp-token
			    :props :pp-number
			    :value (if (cpp-macro-defined name) "1" "0")
			    :state (cpp-token-state name)
			    :next (cpp-token-next name)
			    :file (cpp-token-file name)
			    :line (cpp-token-line name)
			    :colmn (cpp-token-colmn name))
			   acc)
		      nstate)
	      (values nil state)))
	(.do ((token (token-input state)))
	  (values (enq token acc) nstate))))

(defun //cpp-parse-line
    (state &optional (acc (makeq 'cpp-token)) (n-args 0) (args nil))
  #+nil
  (print `(//cpp-parse-line :state=> ,state :acc=> ,acc :n-args=> ,n-args
			    :args=> ,args))
  (/many* state #'//cpp-line-elt acc n-args args))

(defun cpp-main (state)
  (setf *cpp-macro-hash* (make-hash-table :test #'equal))
  #+nil
  (print `(cpp-main ,state))
  (let ((input (mktis :i (=cpp-file state))))
    #+nil
    (print `(@@@ cpp-main ,input))
    (multiple-value-bind (acc _)
	(=/parse-cpp-directive input (makeq 'cpp-token))
      (declare (ignore _))
      #-nil
      (loop :for token :across acc
	    :collect (cpp-token-value token) :into list
	    :when (string= (cpp-token-value token) ";")
	      :collect (format nil ":file ~S :line ~D"
			       (cpp-token-file token) (cpp-token-line token))
		:into list
	    :when (string= (cpp-token-value token) ";")
	      :collect (coerce '(#\Newline) 'string) :into list
	    :finally (format t "~%~{~A ~}" list))
      acc)))

(defun invoke-cpp-main (input &optional (builtin t))
  (declare (type string input))
  (with-open-file (stream "builtin.h" :direction :input
				      :element-type `(unsigned-byte 8)
				      :if-does-not-exist :error)
    (let ((src (let* ((len (file-length stream))
		      (buf (make-array len :element-type '(unsigned-byte 8))))
		 (read-sequence buf stream)
		 (if builtin
		     (concatenate 'string
				  (sb-ext:octets-to-string buf
							   :external-format :utf-8)
				  input)
		     input))))
      (cpp-main (mkcis :i src :o 0 :f "stdin")))))
