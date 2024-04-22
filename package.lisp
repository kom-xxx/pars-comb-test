(in-package :cl-user)

(defpackage :alien-helper
  (:use :cl :sb-ext :sb-alien)
  (:export :strtod :strtof))

#+nil
(defpackage :array-simplify
  (:nicknames :asimp)
  (:use :cl)
  (:export :array-simplify))

(defpackage :c-syntax-type-definisions
  (:nicknames :cs-type-definisions :cs-deftype)
  (:use :cl)
  (:export
   :queue :text-position :parser :parser-ret
   :char-input-parser :many-item-parser :many-char-parser :many-ctype-parser
   :basic-parser :simple-parser :char-test-parser :counter-parser
   :string-test-parser :input-state :char-input-state :token-input-state
   :mkdis :mktis :mkis :mkcis :is-i :is-o :is-n :is-l :is-c :is-f
   :token :token-props :token-value
   :cpp-token :make-cpp-token :copy-cpp-token
   :cpp-token-props :cpp-token-value :cpp-token-state :cpp-token-next
   :cpp-token-attr :cpp-token-spaces
   :cpp-token-file :cpp-token-line :cpp-token-colmn
   :cpp-directive :make-cpp-directive :cpp-directive-value :cpp-directive-props
   ))

(defpackage :c-syntax-character-class
  (:nicknames :cs-chara-class :cs-cc)
  (:use :cl)
  (:export :space? :id1st? :idrest? :dec? :oct? :hex? :ctl? :dec1st?))

(defpackage :c-syntax-utils
  (:nicknames :cs-utils)
  (:use :cl :cs-deftype)
  (:export :char-equal* :makeq :cmakeq :enq :cenq :strcat :tail
   :to-simple-string :dump-hash :dump-hash*))

#+nil
(defpackage memoize
  (:nicknames :memo)
  (:shadow :defun)
  (:use :cl)
  (:export #:defun #:memoize #:unmemoize #:reset #:dump-fn-hash))

(defpackage :c-syntax-parser
  (:nicknames :cs-parser)
  (:use :cl :cs-deftype :cs-cc :cs-utils)
  (:export :define-input :char-input :.many :.many-char
	   :.char= :.char-equal :.string= :.string-equal
	   :aif
	   :.any :.do
	   :.many-space :.many-id1st :.many-idrest :.many-digit :.many-octal
	   :.many-hex :.many-ctl :.may-dec1st))

(defpackage :c-preprocessor-lexical-analyzer
  (:nicknames
   :cpp-lexer :cpp-lex)
  (:use
   :cl :cs-deftype :cs-cc :cs-utils :cs-parser :alien-helper)
  (:export
   :/whitespace* :/whitespace :/unknown-charcter-sequence :/three-dots
   :/string-literal :/semicolon :/rsh-assign :/rsh :/rparen :/rbraket
   :/rbrace :/question :/punctuator :/pp-token :/pp-token* :/pp-number
   :/point-to :/plus-assign :/plus :/newline :/ne :/mul-assign :/mul
   :/mod-assign :/mod :/minos-assign :/minos :/lt :/lsh-assign
   :/lsh :/lparen :/lognot :/logior :/logand :/le :/lbraket
   :/lbrace :/integer-constant :/incr :/identifier
   :/hexadecimal-floating-constant :/header-name :/hash-hash :/hash
   :/gt :/ge :/floating-constant :/eq :/dot :/div-assign :/div
   :/decr :/decimal-floating-constant :/comment :/comma :/colon
   :/character-constant :/bitxor-assign :/bitxor :/bitnot
   :/bitior-assign :/bitior :/bitand-assign :/bitand :/assign
   :/%integer-constant :.universal-character-name :.pp-number-rest
   :.line-comment :.identifier-rest :.hexadecimal-fracrional-part
   :.fractional-constant :.exponent-part :.escape :.delimited-string
   :.cpp-delimited-string :.block-comment :.binary-exponent-part
   :/cpp-define :/cpp-defined :/cpp-elif :/cpp-else :/cpp-endif
   :/cpp-error :/cpp-if :/cpp-ifdef :/cpp-ifndef :/cpp-include
   :/cpp-line :/cpp-pragma :/cpp-undef
   ))


(defpackage :c-expression
  (:nicknames :c-expr)
  (:use :cl :cs-deftype :cs-utils :cs-parser :cpp-lex)
  (:export
   :c-expression :expr-props :expr-value :expr-lhs :expr-rhs
   :token-input :cpp-eval-expr :eval-constant-expr
   :/$abstract-declarator :/$additive-expression :/$alignment-specifier
   :/$argument-expression-list :/$assignment-expression
   :/$atomic-type-specifier :/$bitand-expression :/$bitior-expression
   :/$bitxor-expression :/$cast-expression :/$conditional-expression
   :/$constant-expression :/$declaration :/$declaration-specifiers
   :/$declarator :/$designation :/$designator :/$designator-list
   :/$direct-abstruct-declarator :/$direct-declarator :/$enum-specifier
   :/$enumerator :/$enumerator-list :/$equality-expression :/$expression
   :/$function-specifier :/$generic-assoc-list :/$generic-association
   :/$generic-selection :/$identifier-list :/$init-declarator
   :/$init-declarator-list :/$initializer :/$initializer-list
   :/$logand-expression :/$logior-expression :/$macro-arg
   :/$macro-arg-element :/$macro-arg-element-list
   :/$macro-arg-element-token :/$macro-arg-item :/$macro-arg-item-list
   :/$macro-arg-list :/$multiplicative-expression :/$parameter-declaration
   :/$parameter-list :/$parameter-type-list :/$pointer
   :/$postfix-expression :/$primary-expression :/$relational-expression
   :/$shift-expression :/$specifier-qualifier-list
   :/$static_assert-declaration :/$storage-class-specifier
   :/$struct-declaration :/$struct-declaration-list :/$struct-declarator
   :/$struct-declarator-list :/$struct-or-union-specifier :/$type-name
   :/$type-qualifier :/$type-qualifier-list :/$type-specifier
   :/$typedef-name :/$unary-expression
   ://string-literal ://int ://bitand ://decr ://enum ://incr
   ://integer-constant ://question ://character-constant ://_noreturn
   ://bitior-assign ://rsh ://le ://assign ://rparen ://hash
   ://_alignof ://minos ://else ://lsh-assign ://lbraket-1
   ://plus-assign ://div ://bitnot ://const ://hash-1 ://restrict
   ://union ://switch ://rbraket-1 ://mod-assign ://rbrace
   ://_thread_local ://__var_args__ ://lbraket ://point-to ://sizeof
   ://eq ://lparen ://break ://long ://bitxor ://while ://inline
   ://lbrace ://logior ://_complex ://struct ://three-dots ://for
   ://lognot ://colon ://_alignas ://rbrace-1 ://mul-assign ://auto
   ://void ://bitxor-assign ://identifier ://signed ://lt ://unsigned
   ://hash-hash ://float ://comma ://do ://_generic ://goto
   ://short ://plus ://bitior ://hash-hash-1 ://pp-token ://mul
   ://constant ://logand ://_imaginary ://semicolon ://bitand-assign
   ://double ://enumeration-constant ://typedef ://case ://register
   ://mod ://ne ://rsh-assign ://floating-constant ://lbrace-1 ://dot
   ://_bool ://minos-assign ://volatile ://gt ://static ://ge
   ://return ://char ://div-assign ://default ://if ://continue
   ://lsh ://rbraket ://_atomic ://_static_assert ://extern
   ;; GCC compat
   ://__inline
   ))

(defpackage :cpp-directive
  (:use :cl :cs-deftype :cs-utils :cs-parser :cpp-lex :c-expr)
  (:export
   :/many :/many* :do-print-/many* :=cpp-file :=/parse-cpp-directive
   :cpp-cond-props :cpp-cond-value :cpp-cond-attr :cpp-cond-spaces
   :cpp-cond-state :cpp-cond-next :cpp-cond-file :cpp-cond-line
   :cpp-cond-colmn :cpp-cond-test
   :cpp-def-props :cpp-def-value :cpp-def-replace :cpp-def-params
   :cpp-def-param-hash :cpp-def-three-dots))

(defpackage :c-statement
  (:nicknames :c-stmt)
  (:use :cl :cs-deftype :cs-utils :cs-parser :cpp-lex :c-expr :cpp-directive)
  (:export
   :init-builtins
   :c-declaration :decl-props :decl-value :decl-body
   :c-array-index :cai-props :cai-value :cai-index
   :c-statement :stmt-props :stmt-value :stmt-body
   :c-func-declaration :func-props :func-value :func-spec :func-body :func-oarg
   :/!translation-unit))

(defpackage :c-analyser
  (:nicknames :c-anl)
  (:use
   :cl :cs-deftype :cs-utils :cs-parser :cpp-lex :c-expr :cpp-directive
   :c-stmt))
