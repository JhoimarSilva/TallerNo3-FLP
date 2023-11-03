#lang eopl

#|
Estiven Andrés Martínez Granados:2179687
Jhoimar Silva Torres:2177167
https://github.com/JhoimarSilva/TallerNo3-FLP.git

Definición BNF 

<programa> :=  <expresion>
               un-programa (exp)

<expresion> := <numero>
               numero-lit (num)
            
            := "\""<texto> "\""
               texto-lit (txt)
            
            := <identificador>
               var-exp (id)
             
            := (<expresion> <primitiva-binaria> <expresion>)
               primapp-bin-exp (exp1 prim-binaria exp2)
              
            := <primitiva-unaria> (<expresion>)
               primapp-un-exp (prim-unaria exp)


<primitiva-binaria>
  :=  + (primitiva-suma)
  :=  ~ (primitiva-resta)
  :=  / (primitiva-div)
  :=  * (primitiva-multi)
  :=  concat (primitiva-concat)

<primitiva-unaria>
  :=  longitud (primitiva-longitud)
  :=  add1 (primitiva-add1)
  :=  sub1 (primitiva-sub1)

Extencion de la gramatica:

            := Si <expresion> entonces <expresion> sino <expression> finSi
               condicional-exp (test-exp true-exp false-exp)
            
            := declarar (<identificador> = <expresion> (;)) {<expresion>}
               variableLocal-exp (ids exps cuerpo)

            := procedimiento (<identificador>* ',') haga <expresion> finProc
               prodecimiento-exp (ids cuerpo)

            := evaluar <expresion> (<expresion> ',')* finEval
               app-exp (exp epxs) 

Tenga en cuenta que:
<numero>: Debe definirse para valores decimales y enteros (positivos y negativos)
<texto>: Debe definirse para cualquier texto escrito en racket
<identificador>: En este lenguaje todo identificador iniciará con el símbolo  @, es decir las variables @x y @z son válidas

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
    (expresion ("si" expresion "entonces" expresion "sino" expresion "finSI")
               condicional-exp)
    (expresion ("declare" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)    
    (expresion ("procedure" "(" (separated-list identifier ",") ")" "haga" expresion "finProc")
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

;;Punto 2
;; Define un ambiente inicial con variables (@a, @b, @c, @d, @e) y sus valores correspondientes.
(define initial-env '(@a 1 @b 2 @c 3 @d "hola" @e "FLP"))

;; Función que busca un símbolo en un ambiente dado.
(define (buscar-variable symbol env)
  ;; Función interna que realiza la búsqueda en el ambiente.
  (define (buscar symbol env)
    ;; Caso base: si el ambiente está vacío, devuelve un mensaje de error.
    (cond
      [(null? env) ("Error, la variable no existe")]
      ;; Si el símbolo coincide con el primer elemento del ambiente.
      [(equal? symbol (car env)) 
       ;; Verifica si el ambiente tiene un número impar de elementos (indicando un error).
       (if (odd? (length env))
           ("Error, el valor de la variable está faltante")
           ;; Si tiene un número par de elementos, devuelve el valor asociado con el símbolo.
           (cadr env))]
      ;; Si el símbolo no coincide con el primer elemento del ambiente, realiza una llamada recursiva omitiendo los primeros dos elementos.
      [else (buscar symbol (cddr env))]))
  ;; Llama a la función interna con el símbolo y el ambiente proporcionados como argumentos iniciales.
  (buscar symbol env))

;; Ejemplos de uso de la función buscar-variable (descomenta para probar)
;; (buscar-variable '@a initial-env) ; Devuelve 1
;; (buscar-variable '@b initial-env) ; Devuelve 2
;; (buscar-variable '@e initial-env) ; Devuelve "FLP"

;;Punto 3
;;En una expresión numérica, 0 es falso, cualquier otro caso es verdadero. Para esto diseñe la función valor-verdad? que realiza esta verificación.

(define (valor-verdad? exp)
  (not (= exp 0)))

;; Pruebas
(valor-verdad? 0)  ; salida: #f (false)
(valor-verdad? 1)  ; salida: #t (true)
(valor-verdad? 10) ; salida: #t (true)


;; Definición de una nueva sintaxis llamada expresion usando syntax-rules.
(define-syntax expresion
  (syntax-rules ()
    ;; Regla para manejar la expresión condicional (Si ... entonces ... sino ... finSI).
    [(_ (Si $cond entonces $true_exp sino $false_exp finSI))
     (if $cond $true_exp $false_exp)]))

;; Pruebas de la macro expresion.
(expresion (Si (+ 2 3) entonces 2 sino 3 finSI)) ; Debe imprimir 2
(expresion (Si (= (string-length "d") 4) entonces 2 sino 3 finSI)) ; Debe imprimir 3

;;Definición de una nueva sintaxis llamada declarar usando syntax-rules.

(define-syntax declarar
  (syntax-rules ()
    [(_ (declarar (($id $exp)) $body ...))
     (let (($id $exp)) $body ...)]))

; Pruebas de declaraciones de variables locales


