#lang racket

(define empty-env
  (lambda (var)
    (error "No bind")))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (apply-env env var)
  (env var))

(define init-env empty-env)

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var val Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; Escopo Dinâmico
; proc-val :: Var x Expr -> Proc
#;(define (proc-val var exp)
  (lambda (val Δ)
    (value-of exp (extend-env var val Δ))))

; apply-proc :: Proc x ExpVal x Env -> ExpVal
#;(define (apply-proc proc val Δ)
  (proc val Δ))

(define (value-of exp Δ)
  (define type (car exp))
  (cond [(equal? type 'lit) (cadr exp)]
        [(equal? type 'var) (apply-env Δ (cadr exp))]
        [(equal? type 'dif) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]
        [(equal? type 'let) (value-of (cadddr exp) (extend-env (cadr exp) (value-of (caddr exp) Δ)  Δ))]
        [(equal? type 'if) (if (value-of (cadr exp) Δ)
                               (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]
        [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ)]
        [(equal? type 'call) (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        [(equal? type 'classes) (add-classes (cdr exp) Δ)]
        [else (error "operação não implementada")]))

; Especificação do comportamento de programas
(define (value-of-program prog)
  (value-of (cadr prog) init-env))
;Definição da semantica de classes
;'(classes '(class A extends object '(Fields) '(Methods)) '(class B extends A '(Fields) '(Methods)))

;Comecei a escrever daqui pra baixo vvvvvvvvvvvvvvv

;Trocar env é uma lista
(define empty-env-class '('(object)))

(define (extend-env-class var value env)
  (list var value env)
  )

(define (apply-env-class env var)
  (if (equal? 'empty-env-class (car env)) #f
      (if (equal? var (cadr env)) (caddr env)
          (apply-env-class (cadddr env) var))
      )
  )

(define (value-of-fields cls env)
(if (empty? cls) '()
    (extend-env-class (car cls) 0 (value-of-fields (cdr cls) env))
    ))

(define (value-of-methods cls env)
2
  )

;Coloca cada valor de fields com valor nulo
(define (value-of-definition cls env)
(if (empty? cls) env
  (extend-env-class 'fields (value-of-fields (car cls) env) env))
  )

;'(class A extends object ... )
;Associa '(A object) com Fields e Methods
(define (value-of-class cls env)
 (extend-env-class (list (cadr cls) (cadddr cls)) (value-of-definition (cddddr cls) env) env)
  )

;'(classes '(A) '(B))
; Add a classe A no env em que Add B no env vazio('(object))

(define (add-classes cls env)
  (if (empty? cls) env
  (list (value-of-class (car cls) env) (add-classes (cdr cls) env))))

;(define x1 '(classes (class classe1 extends object (a b c) ))
;  )

(define x1 '(classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f)) ))
(value-of x1 empty-env-class)

(define x2 '(classes (class classe1 extends object (a b c))))
;(value-of x2 empty-env-class)

;((classe1 classe2) (fields (a 0 (b 0 (c 0 ()))) ('(object))) ('(object)))
                   
