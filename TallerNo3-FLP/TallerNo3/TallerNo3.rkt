#lang eopl

#|
Estiven Andrés Martínez Granados:2179687
Jhoimar Silva Torres:2177167
https://github.com/JhoimarSilva/TallerNo3-FLP.git
|#

;; Definición de las funciones zero, isZero?, successor y predecessor
(define (zero) 0)
(define (isZero? n) (= n 0))
(define (successor n) (+ n 1))
(define (predecessor n) (- n 1))

;;Especificación Léxica
(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier
     ("@" letter (arbno (or letter digit "?"))) symbol)
    (identifier
     ("haga" symbol) ("finProc" symbol) )
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
    (expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)
    (expresion ("llamar" identifier "(" (separated-list expresion ",") ")" "finLlamada")
               llamada-recursiva-exp)
    (expresion ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expresion "finProc")
               procedimiento-exp) ;; Nueva regla para procedimientos
    (expresion ("haga" expresion "finProc") ; Nueva regla para "haga"
               haga-exp)
     (expresion ("evaluar" expresion (separated-list expresion ",") "finEval")
               app-exp)
    (expresion ("let" "{" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "}" "in" expresion)
               letrec-exp)
    (expresion
     ("(" expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("llamar" identifier "(" (separated-list expresion ",") ")" "finLlamada") llamada-recursiva-exp)


    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ))


;;Punto2
;; Define un ambiente inicial con variables (@a, @b, @c, @d, @e) y sus valores correspondientes.
(define ambiente '(@a 1 @b 2 @c 3 @d "hola" @e "FLP"))


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

;;Punto 4
;;En una expresión numérica, 0 es falso, cualquier otro caso es verdadero. Para esto diseñe la función valor-verdad? que realiza esta verificación.

(define (valor-verdad? exp)
  (not (= exp 0)))

;; Pruebas
(valor-verdad? 0)  ; salida: #f (false)
(valor-verdad? 1)  ; salida: #t (true)
(valor-verdad? 10) ; salida: #t (true)

;;Punto 4
;; Definición de una nueva sintaxis llamada expresion usando syntax-rules.
(define-syntax expresion
  (syntax-rules ()
    ;; Regla para manejar la expresión condicional (Si ... entonces ... sino ... finSI).
    [(_ (Si $cond entonces $true_exp sino $false_exp finSI))
     (if $cond $true_exp $false_exp)]))

;; Pruebas de la macro expresion.
(expresion (Si (+ 2 3) entonces 2 sino 3 finSI)) ; Debe imprimir 2
(expresion (Si (= (string-length "d") 4) entonces 2 sino 3 finSI)) ; Debe imprimir 3

;;Punto 5
;;Definición de una nueva sintaxis llamada declarar usando syntax-rules.

(define-syntax declarar
  (syntax-rules ()
    [(_ ((@id $exp) . $resto) $cuerpo)
     (let ((@id $exp))
       (declarar $resto $cuerpo))]
    [(_ () $cuerpo)
     $cuerpo]))

(define-syntax expresion2
  (syntax-rules ()
    [(_ (declarar $decls) $cuerpo)
     (declarar $decls $cuerpo)]
    [(_ $exp)
     $exp]))
; Pruebas de declaraciones de variables locales

(expresion2(declarar ((@x 2) (@y 3) (@a 7))(+ @a (- @x @y))))

(expresion2(declarar ((@x 2) (@y 3) (@a 7))(+ @a (- @x @y))))

;;Punto6

(define (expresion? exp)
  (cond
    ((number? exp) #t)  ;; Verifica si es un número
    ((string? exp) #t)  ;; Verifica si es una cadena
    ((symbol? exp) #t)  ;; Verifica si es un símbolo
    ((and (list? exp)
          (>= (length exp) 3)
          (symbol? (car exp))
          (eq? (car exp) 'procedimiento-ex))
     (and (list? (cadr exp))
          (all-symbol? (cadr exp))
          (expresion? (caddr exp))))
    (else #f)))

(define-datatype procVal procVal?
  (procval
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente)))

(define (evaluar-procedimiento exp env)
  (let* ([params (car exp)]
         [body (cadr exp)])
    (procval params body env)))


;;Verifica si el número de parámetros y argumentos coincide
(define (extend-env params args env)
  (if (= (length params) (length args))
      (cons (append params args) env) ; Extiende el ambiente con los nuevos parámetros y argumentos
      ("error Número incorrecto de argumentos")))

;;----------------------------------------------Funciones auxiliares---------------------------------------------

;;Función auxiliar de filtro
(define (my-filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst))
     (cons (car lst) (my-filter pred (cdr lst)))]
    [else
     (my-filter pred (cdr lst))]))

;;Funcion auxiliar para negar el predicado
(define (negate pred)
  (lambda (x) (not (pred x))))

;;Funcion para verificar que los elementos de una lista sean simbolos
(define (all-symbol? lst)
  (define (is-symbol? x)
    (symbol? x))
  (and (list? lst)
       (not (my-filter (negate is-symbol?) lst))))


;;Funcion de my-ormap
(define (my-ormap proc lst)
  (cond
    [(null? lst) #f]
    [(proc (car lst)) #t]
    [else (my-ormap proc (cdr lst))]))
;;-------------------------------------------------------------------------------------------------------------

;;Punto7 y 8
;; Función para evaluar expresiones en un ambiente dado
(define (evaluar-expresion exp env)
  (cond
    [(number? exp) exp]
    [(string? exp) exp]
    [(symbol? exp) (buscar-variable exp env)] ;; Utiliza tu función buscar-variable aquí
    [(list? exp) 
     (case (car exp)
  [("+" "~" "/" "*")
   (let* ([op (car exp)]
          [arg1 (evaluar-expresion (cadr exp) env)]
          [arg2 (evaluar-expresion (caddr exp) env)])
     (case op
       [("+" ) (+ arg1 arg2)]
       [("~") (- arg1 arg2)]
       [("/") (/ arg1 arg2)] ;; Cambiado de "\/" a "/"
       [("*") (* arg1 arg2)] ;; Agregado caso para la multiplicación
       [("longitud")
        (string-length (evaluar-expresion (cadr exp) env))]
       [("concat")
        (string-append (evaluar-expresion (cadr exp) env)
                       (evaluar-expresion (caddr exp) env))]))])])

  [("llamar"
    (let* ([nombre-funcion (cadr exp)]
           [args (cddr exp)])
      (define funcion (buscar-variable nombre-funcion env)) ;; Obtén la función del ambiente
      (if (procVal? funcion) ;; Verifica si es una función
          (apply (procVal? funcion) args) ;; Aplica la función con los argumentos
          ("Error: No se encontró la función o no es válida"))))])


;; Función para evaluar una aplicación de procedimiento
(define (evaluar-app-exp exps env)
  (let* ([proc (evaluar-expresion (car exps) env)]
         [args (map (lambda (arg) (evaluar-expresion arg env)) (cdr exps))])
    (apply proc args)))

;; Evaluar procedimiento con la nueva regla
(define (evaluar-procedimiento1 exp env)
  (let* ([params (car exp)]
         [body (cadr exp)]
         [args (cddr exp)]
         [new-env (extend-env params args env)])
    (evaluar-expresion body new-env)))


;; Ejemplos de uso
         '(declarar (@x=2 @y=3 @a=procedimiento (@x,@y,@z) haga ((@x+@y)+@z) finProc)
                    {evaluar @a (1,2,@x) finEval})

          '(declarar (@x=procedimiento (@a,@b) haga ((@a*@a) + (@b*@b)) finProc;
                     @y=procedimiento (@x,@y) haga (@x+@y) finProc)
                    {(evaluar @x (1,2) finEval) + (evaluar @y (2,3) finEval)})

          '(declarar (@x=Si (@a*@b) entonces (@d concat @e) sino longitud((@d concat @e)) finSI;
                     @y=procedimiento (@x,@y) haga (@x+@y) finProc)
                    {(longitud(@x) * evaluar @y (2,3) finEval)})

#|

;;Utilización del lenguaje

;;A)
(define (areaCirculo radio)
  (let ((pi 3.14159265359)) ; Definimos una variable local para el valor de Pi
    (let ((area (* pi (* radio radio)))) ; Calculamos el área del círculo
      area))) ; Devolvemos el área como resultado de la función

;; Ejemplo de uso:
(define radio 2.5)
(display (areaCirculo radio))

B)



C)
(define (sumar n m)
  
  (if (isZero? m)
      n
      (sumar (successor n) (predecessor m))))

;; Llamada a la función recursiva
(define resultado (sumar 4 5))
(display resultado)

D)



E)

(define (decorador-hola prefix)
  (define (decorar proc)
    (define (decorado)
      (string-append prefix (proc)))
    decorado)
  decorar)

(define (integrantes)
  "Estiven Martinez Y Jhoimar Silva")

(define saludar
  (decorador-hola "Hola:"))

(define decorate
  (saludar integrantes))

(display (decorate)) ; Deberá retornar "Hola: Estiven Martinez y Jhoimar Silva"

|#
