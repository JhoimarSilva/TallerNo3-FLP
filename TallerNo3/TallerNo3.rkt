#lang racket

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
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))









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


