#lang racket

#|
Estiven Andrés Martínez Granados:2179687-2177167-1925648
Jhoimar Silva Torres:2177167
https://github.com/JhoimarSilva/TallerNo3-FLP.git
|#

; Ambiente
(define init-env
  (extend-env
    (list '@a '@b '@c '@d '@e)
    (list 1 2 3 "hola" "FLP")
    (empty-env)))

; Buscar una variable en el ambiente
(define (buscar-variable id env)
  (apply-env env id))

; Función para verificar si un valor es verdadero
(define (valor-verdad? val)
  (not (zero? val)))

; Definición de tipos de datos
(define (crear-valor tipo valor)
  (list tipo valor))

(define (tipo-valor valor)
  (car valor))

(define (contenido-valor valor)
  (cadr valor))

(define (parse-programa exp)
  (list 'programa exp))

(define (parse-numero-lit num)
  (list 'numero num))

(define (parse-texto-lit txt)
  (list 'texto txt))

(define (parse-var-exp id)
  (list 'identificador id))

(define (parse-primapp-bin-exp exp1 prim exp2)
  (list 'primitiva-binaria exp1 prim exp2))

(define (parse-primapp-un-exp prim exp)
  (list 'primitiva-unaria prim exp))

(define (parse-primitiva-binaria prim)
  (case prim
    ['+ 'primitiva-suma]
    ['- 'primitiva-resta]
    ['/ 'primitiva-div]
    ['* 'primitiva-multi]
    ['concat 'primitiva-concat]))

(define (parse-primitiva-unaria prim)
  (case prim
    ['longitud 'primitiva-longitud]
    ['add1 'primitiva-add1]
    ['sub1 'primitiva-sub1]))

; Estructura para representar un ambiente
(define-struct env (variables valores entorno))

; Función para crear un ambiente vacío
(define (empty-env)
  (make-env '() '() #f))

; Función para extender un ambiente con nuevas variables y valores
(define (extend-env ids vals env)
  (make-env (append ids (env-variables env)) (append vals (env-valores env)) env))

; Función para aplicar un ambiente a un identificador y buscar su valor
(define (apply-env env id)
  (define (search id variables valores)
    (cond
      [(empty? variables) (error "Variable no encontrada")]
      [(eq? id (car variables)) (car valores)]
      [else (search id (cdr variables) (cdr valores))]))
  (search id (env-variables env) (env-valores env)))

; Función para evaluar un programa
(define (evaluar-programa programa)
  (evaluar-expresion programa init-env))

; Función para evaluar expresiones
(define (evaluar-expresion exp env)
  (match exp
    [(list 'numero num) num]
    [(list 'texto txt) txt]
    [(list 'identificador id) (buscar-variable id env)]
    ; Agregar casos para otras estructuras sintácticas
    [else (error "Expresión no válida")]))

(define-struct procedimiento (ids cuerpo env))

(define (evaluar-condicional-exp exp entorno)
  (if (valor-verdad? (evaluar-expresion (cadr exp) entorno))
      (evaluar-expresion (caddr exp) entorno)
      (evaluar-expresion (cadddr exp) entorno)))

(define (evaluar-variableLocal-exp exp entorno)
  (let ([nuevos-ids (cadr exp)]
        [expresiones (caddr exp)]
        [cuerpo (cadddr exp)])
    (let ([nuevos-vals (evaluar-secuencia expresiones entorno)])
      (let ([nuevo-entorno (extend-env nuevos-ids nuevos-vals entorno)])
        (evaluar-expresion cuerpo nuevo-entorno)))))

(define (evaluar-procedimiento-exp exp entorno)
  (let ([ids (cadr exp)]
        [cuerpo (caddr exp)])
    (make-procedimiento ids cuerpo entorno)))

(define (evaluar-secuencia expresiones entorno)
  (cond
    [(empty? expresiones) '()]
    [else (cons (evaluar-expresion (car expresiones) entorno)
                (evaluar-secuencia (cdr expresiones) entorno))]))

(define (evaluar-llamar-exp exp entorno)
  (let ([nombre (cadr exp)]
        [argumentos (cddr exp)])
    (let ([proc (buscar-variable nombre entorno)])
      (if (procedimiento? proc)
          (let ([valores (evaluar-secuencia argumentos entorno)])
            (let ([nuevo-entorno (extend-env (procedimiento-ids proc) valores (procedimiento-env proc))])
              (evaluar-expresion (procedimiento-cuerpo proc) nuevo-entorno)))
          (error "No se proporcionó un procedimiento válido")))))

(define (evaluar-expresion exp entorno)
  (match exp
    [(list 'numero num) num]
    [(list 'texto txt) txt]
    [(list 'identificador id) (buscar-variable id entorno)]
    [(list 'condicional-exp test then else) (evaluar-condicional-exp exp entorno)]
    [(list 'variableLocal-exp ids expresiones cuerpo) (evaluar-variableLocal-exp exp entorno)]
    [(list 'procedimiento-ex ids cuerpo) (evaluar-procedimiento-exp exp entorno)]
    [(list 'llamar-exp nombre argumentos) (evaluar-llamar-exp exp entorno)]
    ; Agregar más casos para otras estructuras sintácticas
    [else (error "Expresión no válida")]))

 #|Ejemplo de uso
(define programa-ejemplo
  (parse-programa
   (parse-primapp-bin-exp
    (parse-numero-lit 5)
    '+
    (parse-var-exp '@a))))

(display (evaluar-programa programa-ejemplo))|#

;Implementación de los booleanos

(define (valor-verdad? val)
  (not (zero? val))
)

;;Extensión de la gramática con condicionales

(define (evaluar-expresion exp entorno)
  (cond
    ; Otros casos
    [(and (list? exp) (eq? (car exp) 'condicional-exp))]
     (if (valor-verdad? (evaluar-expresion (cadr exp) entorno))
         (evaluar-expresion (caddr exp) entorno)
         (evaluar-expresion (cadddr exp) entorno)))
    ; Otros casos
    [else (error "Expresión no válida")]
)

;; Implemente declaración de variables locales:

(define (evaluar-expresion exp entorno)
  (cond
    ; Otros casos
    [(and (list? exp) (eq? (car exp) 'variableLocal-exp))]
     (let ([nuevos-ids (cadr exp)]
           [expresiones (caddr exp)]
           [cuerpo (cadddr exp)])
       (let ([nuevos-vals (eval-rands expresiones entorno)])
         (let ([nuevo-entorno (extend-env nuevos-ids nuevos-vals entorno)])
           (evaluar-expresion cuerpo nuevo-entorno))))
    ; Otros casos
    [else (error "Expresión no válida")]
    )
)

;;Extienda la gramática para crear procedimientos

(define (evaluar-expresion exp entorno)
  (cond
    ; Otros casos
    [(and (list? exp) (eq? (car exp) 'procedimiento-ex))]
     (let ([ids (cadr exp)]
           [cuerpo (caddr exp)])
       (procedimiento ids cuerpo entorno))
    ; Otros casos
    [else (error "Expresión no válida")]
    )
)

;;Extienda la gramática para evaluar procedimientos:

(define (parse-procedimiento ids body)
  (list 'procedimiento ids body))

; Definir un datatype para la cerradura (ProcVal)
(define-struct procVal (lista-ID exp amb))

; Extender la función evaluar-expresion para manejar procedimientos
(define (evaluar-procedimiento procedimiento env)
  (make-procVal procedimiento env))

(define (evaluar-app-exp app env)
  (define (buscar-en-ambientes ids vals env)
    (cond
      [(empty? ids) (error "Argumentos insuficientes")]
      [else (local-define (car ids) (car vals) env)
            (buscar-en-ambientes (cdr ids) (cdr vals) env)]))
  (let ([proc (evaluar-expresion (app-proc app) env)]
        [args (map (lambda (arg) (evaluar-expresion arg env)) (app-args app))])
    (match proc
      [(struct procVal (ids body proc-env))
       (local-define 'return #f env)
       (buscar-en-ambientes ids args proc-env)
       (evaluar-expresion body env)
       (lookup 'return env)]
      [else (error "No se proporcionó un procedimiento válido")])))

; Extienda la gramática para incluir llamados recursivos. Proponga una definición en la gramática e impleméntela.

<expresion> := <numero>
             := "\""<texto> "\""
             := <identificador>
             := (<expresion> <primitiva-binaria> <expresion>)
             := <primitiva-unaria> (<expresion>)
             := Si <expresion> entonces <expresion> sino <expresion> finSI
             := declarar (<identificador> = <expresion>) { <expresion> }
             := procedimiento (<identificador>* ',' ') haga <expresion> finProc
             := evaluar <expresion> (expresion ',' ')* finEval
             := llamar <identificador> (expresion ',' ')* finLlamar



