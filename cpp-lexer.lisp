(in-package :cpp-lex)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimize-decl*
    '(declare (optimize (speed 3) (safety 0) (debug 0)))))

(defvar *current-line-state* :newline)
(defvar *current-processing-state* :preprocessor)

(defvar *c-keyword-list*
  '("_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex" "_Generic" "_Imaginary"
    "_Noreturn" "_Static_assert" "_Thread_local"
    "auto" "break" "case" "char" "const" "continue" "default" "do" "double"
    "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long"
    "register" "restrict" "return" "short" "signed" "sizeof" "static" "struct"
    "switch" "typedef" "union" "unsigned" "void" "volatile" "while"
    ;; GCC compat
    "__inline"))

(defvar *c-keyword-hash* (make-hash-table :test #'equal))

(loop :for elt :in *c-keyword-list*
      :do (setf (gethash elt *c-keyword-hash*)
		(intern (format nil "~:@(C-KEYWORD-~A~)" elt) :keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cpp-directive-list*
    '("ifndef" "ifdef" "if" "elif" "else" "endif" "include" "define" "undef"
      "line" "error" "pragma" "defined"))

  (defvar *cpp-directive-hash* (make-hash-table :test #'equal))

  (loop :for elt :in *cpp-directive-list*
	:do (setf (gethash elt *cpp-directive-hash*)
		  (intern (format nil "~:@(CPP-DIRECTIVE-~A~)" elt) :keyword))))

(declaim (ftype simple-parser .universal-character-name))
(defun .universal-character-name (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (flet ((legal-ucn (ucn nstate)
	   (let ((code (parse-integer ucn :start 2 :radix 16)))
	     (declare (type (integer 0 #x110000) code))
	     (if (or (and (< code #xa0) (/= code #x24 #x40 #x60))
		     (<= #xd800 code #xdfff))
		 (values nil nil)
		 (values (enq (code-char code) acc) nstate)))))
    (declare (inline legal-ucn))
    (.do ((bksl (.char= state #\\ (makeq 'character)))
	  (cu (.char-equal nstate #\u bksl))
	  (elt (.many-hex nstate cu 4 4)))
      (if (char= (char elt 1) #\u)
	  (legal-ucn elt nstate)
	  (.do ((elt (.many-hex nstate elt 4 4)))
	    (legal-ucn elt nstate))))))

(declaim (ftype simple-parser .identifier-rest))
(defun .identifier-rest (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (multiple-value-bind (elt rst) (.any (.universal-character-name state acc)
				       (.many-idrest state acc))
    (if (= (is-o state) (is-o rst))
	(values acc state)
	(.identifier-rest rst elt))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token)) /identifier))
(defun /identifier (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do (((first nst) (.any (.universal-character-name state)
			     (.many-id1st state (makeq 'character) 1)))
	  ((rest nnst) (.identifier-rest nst first)))
      (if (eq nst nnst)
	  (progn
	    (let ((token (make-cpp-token :props :identifier :value first
					 :state state :next nst
					 :file file :line line :colmn colmn)))
	      (values (enq token acc) nst)))
	  (progn
	    (let ((token (make-cpp-token :props :identifier :value rest
					 :state state :next nnst
					 :file file :line line :colmn colmn)))
	      (values (enq token acc) nnst)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-cpp-directive (name)
    (let ((fn-name (intern (format nil "~:@(/cpp-~A~)" name))))
      `(defun ,fn-name (state &optional (acc (makeq 'cpp-token)))
	 (.do ((ident (/identifier state acc)))
	   (let ((token (tail ident)))
	     (multiple-value-bind (props found)
		 (gethash (cpp-token-value token) *cpp-directive-hash*)
	       (if (null found)
		   (values nil nil)
		   (if (string= (cpp-token-value token) ,name)
		       (progn
			 (setf (cpp-token-props token) props)
			 (values ident nstate))
		       (values nil nil))))))))))

#+nil(define-cpp-directive "define")
#.(loop :for name :in *cpp-directive-list*
	:collect `(define-cpp-directive ,name) :into defs
	:finally (return `(progn ,@defs)))

(declaim (ftype simple-parser .escape))
(defun .escape (state &optional (acc (makeq 'character)))
  (declare (type char-input-state state)
	   (type (queue character) acc))
  #.*optimize-decl*
  (.do ((_ (.char= state #\\) (declare (ignorable  _)))
	(char (char-input nstate)))
    (case char
      ((#\' #\" #\? #\\) (values (enq char acc) nstate))
      (#\a (values (enq #\Bel acc) nstate))
      (#\b (values (enq #\Backspace acc) nstate))
      (#\f (values (enq #\Page acc) nstate))
      (#\n (values (enq #\Newline acc) nstate))
      (#\r (values (enq #\Return acc) nstate))
      (#\t (values (enq #\Tab acc) nstate))
      (#\v (values (enq #\Vt acc) nstate))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 $\7)
       (let ((queue (makeq 'character)))
	 (vector-push-extend char queue)
	 (.do ((octal (.many-hex nstate queue 0 2)))
	   (let ((value (code-char (parse-integer octal :radix 8))))
	     (values (enq value acc) nstate)))))
      (#\x
       (let ((queue (makeq 'character)))
	 (.do ((hex (.many-hex nstate queue)))
	   (let ((value (code-char (parse-integer hex :radix 16))))
	     (values (enq value acc) nstate))))))))

(declaim (ftype char-test-parser .delimited-string))
(defun .delimited-string (state delim &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.do ((string (.any (.escape state acc)
		      (.universal-character-name state acc)
		      (.many-char state
				  (lambda (c)
				    (and (char/= c delim) (char/= c #\\)
					 (char/= c #\Newline)))
				  acc))))
    (multiple-value-bind (c nnstate) (char-input nstate)
      (cond ((or (null c) (char= c #\Newline)) nil)
	    ((char= c delim) (values (cenq c string) nnstate))
	    (t (.delimited-string nstate delim string))))))

(declaim (ftype (function (char-input-state)
			  (parser-ret cpp-token char-input-state))
		/%integer-constant))
(defun /%integer-constant (state)
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (flet ((return-integer-constant (string nstate radix)
             (let* ((value (parse-integer string :start (if (= radix 16) 2 0)
						 :radix radix))
                    (token (make-cpp-token :props :integer-constant
                                           :value value
                                           :state state
                                           :next nstate
					   :file file :line line :colmn colmn)))
               (values token nstate))))
      (.any (.do ((head (.many-char state #'dec1st? (makeq 'character) 1))
                  (rest (.many-digit nstate head)))
              (return-integer-constant rest nstate 10))
            (.do ((zero (.char= state #\0))
                  (x (.char-equal nstate #\X zero))
                  (rest (.many-hex nstate x)))
              (return-integer-constant rest nstate 16))
            (.do ((zero (.char= state #\0))
                  (rest (.many-octal nstate zero)))
              (return-integer-constant rest nstate 8))))))

(declaim (ftype (function (char-input-state)
			  (parser-ret (cons keyword list) char-input-state))
		[integer-suffix))
(defun [integer-suffix (state)
  #.*optimize-decl*
  (.any (.do ((ll (.many-char state (lambda (c) (char-equal c #\L))
                              (makeq 'character) 2 2))
              (llu (.char-equal nstate #\U ll)))
          (values '(:LONG-LONG :UNSIGNED) nstate))
        (.do ((ll (.many-char state (lambda (c) (char-equal c #\L))
                              (makeq 'character) 2 2)))
          (values '(:LONG-LONG) nstate))
        (.do ((l (.char-equal state #\L))
              (lu (.char-equal nstate #\U l)))
          (values '(:LONG :UNSIGNED) nstate))
        (.do ((l (.char-equal state #\L)))
          (values '(:LONG) nstate))
        (.do ((u (.char-equal state #\U))
              (ull (.many-char nstate (lambda (c) (char-equal c #\L)) u 2 2)))
          (values '(:UNSIGNED :LONG-LONG) nstate))
        (.do ((u (.char-equal state #\U))
              (ul (.char-equal nstate #\L u)))
          (values '(:UNSIGNED :LONG) nstate))
        (.do ((u (.char-equal state #\U)))
          (values '(:UNSIGNED) nstate))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/integer-constant))
(defun /integer-constant (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (.any (.do ((integer (/%integer-constant state))
              (suffix ([integer-suffix nstate)))
          (setf (cpp-token-attr integer) suffix)
          (values (enq integer acc) nstate))
        (.do ((integer (/%integer-constant state)))
          (values (enq integer acc) nstate))))

(declaim (ftype simple-parser .fractional-constant))
(defun .fractional-constant (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.any (.do ((integer (.many-digit state acc))
              (dot (.char= nstate #\. integer))
              (fraction (.many-digit nstate dot)))
          (values fraction nstate))
        (.do ((dot (.char= state #\. acc))
              (fraction (.many-digit nstate dot)))
          (values fraction nstate))
        (.do ((integer (.many-digit state acc))
              (dot (.char= nstate #\. integer)))
          (values dot nstate))))

(declaim (ftype simple-parser .exponent-part))
(defun .exponent-part (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.any (.do ((e (.char-equal state #\E acc))
              (sign (.any (.char= nstate #\+ e) (.char= nstate #\- e)))
              (expt (.many-digit nstate sign)))
          (values expt nstate))
        (.do ((e (.char-equal state #\E acc))
              (expt (.many-digit nstate e)))
          (values expt nstate))))

(declaim (ftype (function (char-input-state)
			  (parser-ret (cons keyword list) char-input-state))
                [floating-suffix))
(defun [floating-suffix (state)
  #.*optimize-decl*
  (.any (.do ((suffix (.char-equal state #\F)))
          (values (cons :SINGLE-FLOAT nil) nstate))
        (.do ((suffix (.char-equal state #\L)))
          (values (cons :LONG-FLOAT nil) nstate))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/decimal-floating-constant))
(defun /decimal-floating-constant (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (flet ((return-floating-constant (value nstate suffix)
             (let* ((value (cond
                             ((eq (car suffix) :double-float)
                              (let ((*read-default-float-format* 'double-float))
				(read-from-string value)))
                             ((eq (car suffix) :single-float)
                              (let ((*read-default-float-format* 'single-float))
				(read-from-string value)))
                             ((eq (car suffix) :long-float)
                              (let ((*read-default-float-format* 'double-float))
				(read-from-string value)))))
                    (token (make-cpp-token :props :decimal-floating-point
                                           :value value
                                           :attr suffix
                                           :state state
                                           :next nstate
					   :file file :line line :colmn colmn)))
               (values (enq token acc) nstate))))
      (declare (inline return-floating-constant))
      (.any (.do ((frac (.fractional-constant state))
                  (expt (.exponent-part nstate frac))
                  (suffix ([floating-suffix nstate)))
              (return-floating-constant expt nstate suffix))
            (.do ((frac (.fractional-constant state))
                  (expt (.exponent-part nstate frac)))
              (return-floating-constant expt nstate (cons :double-float nil)))
            (.do ((frac (.fractional-constant state))
                  (suffix ([floating-suffix nstate)))
              (return-floating-constant frac nstate suffix))
            (.do ((frac (.fractional-constant state)))
              (return-floating-constant frac nstate (cons :double-float nil)))
            (.do ((frac (.many-digit state))
                  (expt (.exponent-part nstate frac))
                  (suffix ([floating-suffix nstate)))
              (return-floating-constant expt nstate suffix))
            (.do ((frac (.many-digit state))
                  (expt (.exponent-part nstate frac)))
              (return-floating-constant expt nstate (cons :double-float nil)))))))

(declaim (ftype simple-parser .hexadecimal-fracrional-part))
(defun .hexadecimal-fracrional-part (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.any (.do ((integer (.many-hex state acc))
              (dot (.char= nstate #\. integer))
              (fraction (.many-hex nstate dot)))
          (values fraction nstate))
        (.do ((integer (.many-hex state acc))
              (dot (.char= nstate #\. integer)))
          (values dot nstate))
        (.do ((dot (.char= state #\. acc))
              (fraction (.many-hex nstate dot)))
          (values fraction nstate))))

(declaim (ftype simple-parser .binary-exponent-part))
(defun .binary-exponent-part (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.any (.do ((p (.char-equal state #\P acc))
              (sign (.any (.char= nstate #\+ p) (.char= nstate #\- p)))
              (expt (.many-digit nstate sign)))
          (values expt nstate))
        (.do ((p (.char-equal state #\P acc))
              (expt (.many-digit nstate p)))
          (values expt nstate))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/hexadecimal-floating-constant))
(defun /hexadecimal-floating-constant (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (flet ((return-hexadecimal-floating-constant (value nstate suffix)
             (let* ((value (cond
                             ((null suffix) (strtod value))
                             ((eq (car suffix) :SINGLE-FLOAT) (strtof value))
                             ((eq (car suffix) :LONG-FLOAT) (strtod value))))
                    (token (make-cpp-token :props :hexadecimal-floating-point
                                           :value value
                                           :attr suffix
                                           :state state
                                           :next nstate
					   :file file :line line :colmn colmn)))
               (values (enq token acc) nstate))))
      (.any (.do ((prefix (.string= state "0x"))
                  (fraction (.hexadecimal-fracrional-part nstate prefix))
                  (exponent (.binary-exponent-part nstate fraction))
                  (suffix ([floating-suffix nstate)))
              (return-hexadecimal-floating-constant exponent nstate suffix))
            (.do ((prefix (.string= state "0x"))
                  (fraction (.hexadecimal-fracrional-part nstate prefix))
                  (exponent (.binary-exponent-part nstate fraction)))
              (return-hexadecimal-floating-constant exponent nstate nil))
            (.do ((prefix (.string= state "0x"))
                  (fraction (.many-hex nstate prefix))
                  (exponent (.binary-exponent-part nstate fraction))
                  (suffix ([floating-suffix nstate)))
              (return-hexadecimal-floating-constant exponent nstate suffix))
            (.do ((prefix (.string= state "0x"))
                  (fraction (.many-hex nstate prefix))
                  (exponent (.binary-exponent-part nstate fraction)))
              (return-hexadecimal-floating-constant exponent nstate nil))))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/floating-constant))
(defun /floating-constant (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (.any (/decimal-floating-constant state acc)
        (/hexadecimal-floating-constant state acc)))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/character-constant))
(defun /character-constant (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (labels
      ((test-chara-const (string prefix)
	 (let ((str (subseq string (if prefix 2 1) (1- (length string)))))
	   (if (null prefix)
	       (let* ((len (length str))
		      (cnt (loop :for i :from 1 :below len
				 :count
				 (>= (char-code (char string i)) 256))))
		 (if (or (> len 4) (> cnt 0))
		     nil
		     :base-char))
	       (let ((len (length str)))
		 (if (> len 1)
		     (values nil nil)
		     (cond ((and (char= prefix #\u)
				 (< (char-code (char str 0)) #x10000))
			    :utf-16)
			   ((char= prefix #\U) :utf-32)
			   ((char= prefix #\L) :wide-char)
			   (t nil)))))))
       (return-char-const (string nstate prefix)
	 (let ((prefix (test-chara-const string prefix)))
	   (if (not prefix)
	       (values nil nil)
	       (let ((token (make-cpp-token :props :character-constant
					    :value string
					    :attr prefix
					    :state state
					    :next nstate
					    :file file :line line :colmn colmn)))
		 (values (enq token acc) nstate))))))
    (.do ((1st (.any (.char= state #\')
		     (.char= state #\L)
		     (.char-equal state #\u))))
      (if (char= (char 1st 0) #\')
	  (.do ((rest (.delimited-string nstate #\' 1st)))
	    (return-char-const rest nstate nil))
	  (.do ((2nd (.char= nstate #\' 1st)))
	    (.do ((rest (.delimited-string nstate #\' 2nd)))
	      (return-char-const rest nstate (char 1st 0)))))))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/string-literal))
(defun /string-literal (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (labels ((test-string-literal (str prefix)
	       (if (char= prefix #\u)
		   (let ((cnt (loop :for c :across (to-simple-string str)
				    :count (> (char-code c) #xffff))))
		     (if (> cnt 0) nil t))
		   t))
	     (return-string-literal (string nstate prefix)
	       (if (test-string-literal string prefix)
		   (let* ((prefix (case prefix
				    ((nil) :base-char)
				    (#\L :wide-char)
				    (#\U :utf-32)
				    (#\u :utf-16)
				    (#\8 :utf8)))
			  (token (make-cpp-token :props :string-literal
						 :value string
						 :attr prefix
						 :state state
						 :next nstate
						 :file file :line line :colmn colmn)))
		     (values (enq token acc) nstate))
		   (values nil nil))))
      (.any (.do ((quate (.char= state #\"))
		  (rest (.delimited-string nstate #\" quate)))
	      (return-string-literal rest nstate nil))
	    (.do ((quate (.string= state "L\""))
		  (rest (.delimited-string nstate #\" quate)))
	      (return-string-literal rest nstate #\L))
	    (.do ((quote (.string= state "U\""))
		  (rest (.delimited-string nstate #\" quote)))
	      (return-string-literal rest nstate #\U))
	    (.do ((quote (.string= state "u\""))
		  (rest (.delimited-string nstate #\" quote)))
	      (return-string-literal rest nstate #\u))
	    (.do ((quote (.string= state "u8\""))
		  (rest (.delimited-string nstate #\" quote)))
	      (return-string-literal rest nstate #\8))))))
		  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *c-punctuators-list*
    '(("%:%:" . :hash-hash-1) ("..." . :three-dots)
      ("<<=" . :lsh-assign) (">>=" . :rsh-assign)
      ("->" . :point-to) ("++" . :incr) ("--" . :decr)
      ("<<" . :lsh) (">>" . :rsh)
      ("<=" . :le) (">=" . :ge) ("==" . :eq) ("!=" . :ne)
      ("&&" . :logand) ("||" . :logior)
      ("*=" . :mul-assign) ("/=" . :div-assign) ("%=" . :mod-assign)
      ("+=" . :plus-assign) ("-=" . :minos-assign)
      ("&=" . :bitand-assign) ("^=" . :bitxor-assign) ("|=" . :bitior-assign)
      ("##" . :hash-hash)
      ("<:" . :lbraket-1) (":>" . :rbraket-1)
      ("<%" . :lbrace-1) ("%>" . :rbrace-1)
      ("%:" . :hash-1)
      ("[" . :lbraket) ("]" . :rbraket) ("(" . :lparen) (")" . :rparen)
      ("{" . :lbrace) ("}" . :rbrace)
      ("." . :dot)
      ("&" . :bitand) ("*" . :mul) ("+" . :plus) ("-" . :minos) ("~" . :bitnot)
      ("!" . :lognot)
      ("/" . :div) ("%" . :mod) ("<" . :lt) (">" . :gt)
      ("^" . :bitxor) ("|" . :bitior)
      ("?" . :question) (":" . :colon) (";" . :semicolon) ("=" . :assign)
      ("," . :comma) ("#" . :hash)))

  (defvar *c-punctuators-hash* (make-hash-table :test #'equal))
  (defvar *c-punctuators-rev-hash* (make-hash-table :test #'eq))

  (loop :for (val . kwd) :in *c-punctuators-list*
	:do (progn
	      (multiple-value-bind (_ found)
		  (gethash val *c-punctuators-hash*)
		(declare (ignore _))
		(unless found
                  (setf (gethash val *c-punctuators-hash*) kwd)))
	      (multiple-value-bind (_ found)
		  (gethash kwd *c-punctuators-rev-hash*)
		(declare (ignore _))
		(unless found
		  (setf (gethash kwd *c-punctuators-rev-hash*) val))))))

(defmacro define-punctuator-parser (name)
  (let ((kwd-name (intern (format nil "~:@(~A~)" name) :keyword))
	(alt-name (intern
		   (format nil "~:@(~A-1~)" name) :keyword))
	(fn-name (intern (format nil "~:@(/~A~)" name)))
	(props (intern (format nil "C-PUNCTUATOR-~A" name) :keyword)))
    (multiple-value-bind (kwd-sym found0)
        (gethash kwd-name *c-punctuators-rev-hash*)
      (declare (ignore found0))
      (multiple-value-bind (alt-sym found1)
          (gethash alt-name *c-punctuators-rev-hash*)
	`(progn
	   (declaim (ftype (simple-parser char-input-state (queue cpp-token))
			   ,fn-name))
	   (defun ,fn-name (state &optional (acc (makeq 'cpp-token)))
	     #.*optimize-decl*
             (declare (type char-input-state state)
                      (type (queue cpp-token) acc))
	     (let ((file (is-f state))
		   (line (is-l state))
		   (colmn (is-c state)))
	       (.do ((token ,(if found1
				 `(.any (.string= state ,kwd-sym)
					(.string= state ,alt-sym))
				 `(.string= state ,kwd-sym))))
		 (let ((token (make-cpp-token :props ,props :value token
					      :state state :next nstate
					      :file file :line line
					      :colmn colmn)))
		   (values (enq token acc) nstate))))))))))

#+nil(define-punctuator-parser :hash-hash)
#.(loop :for (_ . kwd) :in *c-punctuators-list*
	:unless (search "-1" (symbol-name kwd))
	:collect `(define-punctuator-parser ,kwd) :into defs
	:finally (return `(progn ,@defs)))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/punctuator))
(defun /punctuator (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (declare (type char-input-state state)
           (type (queue cpp-token) acc))
  (macrolet ((punctuator (state acc)
               (loop :for (_ . kwd) :in *c-punctuators-list*
                     :unless (search "-1" (symbol-name kwd))
                     :collect (let ((fn-name
                                      (intern (format nil "~:@(/~A~)" kwd))))
                                `(,fn-name ,state ,acc)) :into defs
                     :finally (return `(.any ,@defs)))))
    (punctuator state acc)))

(declaim (ftype char-test-parser .cpp-delimited-string))
(defun .cpp-delimited-string (state delim &optional (acc (cmakeq)))
  #.*optimize-decl*
  (declare (type char-input-state state)
           (type character delim)
           (type (queue character) acc))
  (.do ((string
	 (.any (.many-char state
			   (lambda (c) (and (char/= c delim)
                                            (char/= c #\Newline)))
			   acc 1)
	       (.universal-character-name state acc)))
	(delim? (.any (.char= nstate delim string)
		      (.char= nstate #\Newline string))))
    (let ((c (char delim? (1- (length delim?)))))
      (cond ((char= c delim) (values delim? nstate))
	    ((char= C #\Newline) nil)
	    (t (.cpp-delimited-string nstate delim delim?))))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/header-name))
(defun /header-name (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do ((start (.any (.char= state #\") (.char= state #\<))))
      (let ((deiim (if (char= (char start 0) #\") #\" #\>)))
	(.do ((hname (.cpp-delimited-string nstate deiim start)))
	  (let ((token (make-cpp-token :props :header-name :value hname
				       :state state :next nstate
				       :file file :line line :colmn colmn)))
	    (values (enq token acc) nstate)))))))

(declaim (ftype simple-parser .pp-number-rest))
(defun .pp-number-rest (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (multiple-value-bind (rest nstate)
      (.any (.many-digit state acc 1)
            (.do ((e (.char-equal state #\E acc))
                  (sign (.any (.char= nstate #\+ e) (.char= nstate #\- e))))
              (values sign nstate))
            (.do ((p (.char-equal state #\P acc))
                  (sign (.any (.char= nstate #\+ p) (.char= nstate #\- p))))
              (values sign nstate))
            (.many-id1st state acc 1)
            (.char= state #\. acc))
    (if (null rest)
        (values acc state)
        (.pp-number-rest nstate rest))))


(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/pp-number))
(defun /pp-number (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do ((head (.any (.do ((dot (.char= state #\.))
                            (digit (.many-digit nstate dot 1)))
			(values digit nstate))
                      (.do ((digit (.many-digit state (makeq 'character) 1)))
			(values digit nstate))))
          (rest (.pp-number-rest nstate head)))
      (let ((token (make-cpp-token :props :pp-number :value rest :state state
                                   :next nstate
				   :file file :line line :colmn colmn)))
	(values (enq token acc) nstate)))))

(declaim (ftype simple-parser .line-comment))
(defun .line-comment (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.do ((line (.many-char state (lambda (c) (char/= c #\Newline)) acc))
        (nl (.char= nstate #\Newline line)))
    (values nl nstate)))

(declaim (ftype simple-parser .block-comment))
(defun .block-comment (state &optional (acc (makeq 'character)))
  #.*optimize-decl*
  (.do ((string (.many-char state (lambda (c) (char/= c #\*)) acc))
        (delim? (.char= nstate #\* string)))
    (multiple-value-bind (next-char nnstate) (.char= nstate #\/ delim?)
      (declare (type (or (queue character) null) next-char))
      (if (null next-char)
          (.block-comment nstate delim?)
          (values next-char nnstate)))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/comment))
(defun /comment (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (flet ((return-comment (comment nstate attr)
             (let ((token (make-cpp-token :props :comment
                                          :value comment
                                          :attr attr
                                          :state state
                                          :next nstate
					  :file file :line line :colmn colmn)))
	       #+nil(print `(<<< /comment ,token))
               (values (enq token acc) nstate))))
      #+nil(print `(/comment ,state))
      (.any (.do ((start (.string= state "//"))
                  (rest (.line-comment nstate start)))
              (return-comment rest nstate :line-comment))
            (.do ((start (.string= state "/*"))
                  (rest (.block-comment nstate start)))
              (return-comment rest nstate :block-comment))))))


(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/whitespace))
(defun /whitespace (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  #+nil(print `(/whitespace ,state ,acc))
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do ((space (.many-space state)))
      #+nil(print `(@@@ /whitespace ,space))
      (let ((token (make-cpp-token :props :whitespace :value space
                                   :state state :next nstate
				   :file file :line line :colmn colmn)))
	#+nil(print `(<<< /whitespace ,token ,nstate))
	(values (enq token acc) nstate)))))

(declaim (ftype (function (input-state &optional (queue cpp-token)
				       sb-int:index sb-int:index sb-int:index)
                          (values (queue cpp-token) input-state))
	        /whitespace*))
(defun /whitespace* (state &optional (acc (makeq 'cpp-token)) (min 0)
			     (max #.(1- array-dimension-limit)) (cnt 0))
  #.*optimize-decl*
  (declare (type (queue cpp-token) acc))
  #+nil(print '/whitespace*)
  (.many state
	 #'(lambda (state acc)
	     (declare (type input-state state)
		      (type (queue cpp-token) acc))
	     #+nil(print `(/whitespace*-lambda ,(type-of acc)))
	     (.any (/comment state acc) (/whitespace state acc)))
	 acc #+nil(makeq 'cpp-token) min max cnt))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/newline))
(defun /newline (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do ((nl (.char= state #\Newline)))
      (let ((token (make-cpp-token :props :newline :value nl
                                   :state state :next nstate
				   :file file :line line :colmn colmn)))
	(values (enq token acc) nstate)))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/unknown-charcter-sequence))
(defun /unknown-charcter-sequence (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (let ((file (is-f state))
	(line (is-l state))
	(colmn (is-c state)))
    (.do ((ukc (.many-char state
                           (lambda (c)
                             (or (char= c #\$) (char= c #\@) (char= c #\`)))
                           (makeq 'character)
                           1)))
      (let ((token (make-cpp-token :props :unknown-char-sequence :value ukc
                                   :state state :next nstate
				   :file file :line line :colmn colmn)))
	(values (enq token acc) nstate)))))

(declaim (ftype (simple-parser char-input-state (queue cpp-token))
		/pp-token))
(defun /pp-token (state &optional (acc (makeq 'cpp-token)))
  #.*optimize-decl*
  (.any (/identifier state acc)
        (/pp-number state acc)
        (/character-constant state acc)
        (/string-literal state acc)
        (/punctuator state acc)
	(/header-name state acc)
        (/unknown-charcter-sequence state acc)))

(defun /pp-token* (state &optional (acc (makeq 'cpp-token)))
  (declare (ignore acc))
  #.*optimize-decl*
  (.do ((ws (/whitespace* state))
	((token nnstate) (/pp-token nstate)))
    (if (eq nstate nnstate)
	(values nil nil)
	(values (vector-pop token) nnstate))))
