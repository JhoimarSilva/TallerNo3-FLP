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
    (identifier
     ("@" letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." (arbno digit)) number)
    (number
     (digit (arbno digit) "," (arbno digit)) number)
    (string
       ("\"" (arbno (not #\")) "\"") string)
    ))

;;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (string) texto-lit)
    (expresion (identifier) var-exp)
    (expresion ("if" expresion "then" expresion "else" expresion "finSI" expresion "else-if")
               condicional-exp)
    (expresion ("declare" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)    
    (expresion ("procedure" "(" (separated-list identifier ",") ")" "do" expresion "finProc")
               procedimiento-exp)
    (expresion ("assess" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)    
    (expresion ("let" "{" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "}" "in" expresion)
               letrec-exp)
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


(define initial-env '(@a 1 @b 2 @c 3 @d "hola" @e "FLP"))

(define (buscar-variable symbol env)
  (define (buscar symbol env)
    (cond
      [(null? env) ("Error, la variable no existe")]
      [(equal? symbol (car env)) 
       (if (odd? (length env))
           ("Error, el valor de la variable está faltante")
           (cadr env))]
      [else (buscar symbol (cddr env))]))
  (buscar symbol env))

;;(buscar-variable '@a initial-env) ; salida 1
;;(buscar-variable '@b initial-env) ; salida 2
;;(buscar-variable '@e initial-env) ; salida "FLP"





