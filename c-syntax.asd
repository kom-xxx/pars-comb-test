(defsystem c-syntax
  :description "c-syntax, a tribial impremetation of c syntax checker"
  :license "BSD"
  :components
  ((:file "package")
   (:file "alien-helper" :depends-on ("package"))
   #+nil(:file "array-simplify" :depends-on ("package"))
   #+nil(:file "memoize" :depends-on ("package"))
   (:file "deftype" :depends-on ("package"))
   (:file "chara-class" :depends-on ("package"))
   (:file "utils" :depends-on ("package"))
   (:file "parser" :depends-on ("deftype" "chara-class" "utils"))
   (:file "cpp-lexer"
    :depends-on ("deftype" "chara-class" "utils" "alien-helper" "parser"))
   (:file "expression"
    :depends-on ("deftype" "utils" "parser" "cpp-lexer"))
   (:file "cpp-directive"
    :depends-on ("deftype" "utils" "parser" "cpp-lexer" "expression"))
   (:file "statement"
    :depends-on ("deftype" "utils" "parser" "cpp-lexer" "expression"
			   "cpp-directive"))
   (:file "analyser"
    :depends-on ("deftype" "utils" "parser" "cpp-lexer" "expression"
			   "cpp-directive" "statement"))))
