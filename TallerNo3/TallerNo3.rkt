#lang eopl

#|
Estiven Andrés Martínez Granados:2179687
Jhoimar Silva Torres:2177167
https://github.com/JhoimarSilva/TallerNo3-FLP.git
|#

;;Especificación Léxica
(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
  ("%" (arbno (not #\newline))) skip)
   (number
     (digit (arbno digit) "." (arbno digit)) number)
   (number
     (digit (arbno digit) "," (arbno digit)) number)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

;;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) num-exp)
    (expresion (string) texto-lit)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression expression "else-if")
                if-exp)
    (expression ("procedures" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expresion ("assess" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)
    (expresion ("declare" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)    
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    
;;características adicionales
    (expresion
     ("(" expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")") primapp-un-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ))

(define assess-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (string) string)
      (var-exp (id) (buscar-variable env id))
      (condicional-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (assess-expresion test-exp env))
                  (assess-expresion true-exp env)
                  (assess-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)               
                         (let ((values (eval-rands exps env)))                 
                           (let ((extended-env (extend-env ids values env)))                   
                             assess-expresion cuerpo extended-env))))      
      (procedimiento-exp (ids body)
                (cerradura ids body env))
      (app-exp (rator rands)
               (let ((proc (assess-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (primapp-bin-exp (exp1 exp-bin exp2)
                       (let ((val1 (assess-expresion exp1 env))
                             (val2 (assess-expresion exp2 env)))
                         (apply-binary-primitive val1 exp-bin val2)))
      
      (primapp-un-exp (exp-un exp)
                      (let ((val (assess-expresion exp env)))
                        (apply-unary-primitive exp-un val)))))
