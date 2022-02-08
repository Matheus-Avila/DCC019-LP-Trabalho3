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
        ;[(equal? type 'class) ()]
        [else (error "operação não implementada")]))

; Especificação do comportamento de programas
(define (value-of-program prog)
  (value-of (cadr prog) init-env))
;Definição da semantica de classes
;'(classes '(class A extends object '(Fields) '(Methods)) '(class B extends A '(Fields) '(Methods)))

(define (value-of-fields cls env)
1
  )

(define (value-of-methods cls env)
2
  )

;Trocar extend-env para criar uma lista se baseando no exemplo de ambiente procedural
(define (value-of-definition cls env)
(if (empty? cls) env
  (extend-env (car cls) 0 env))
  )

;'(class A extends object ... )
;Associa '(A object) com Fields e Methods
(define (value-of-class cls env)
 (extend-env '((cadr cls) (cadddr cls)) (value-of-definition cls env) env)
  )

;'(classes '(A) '(B))
; Add a classe A no env em que Add B no env vazio('(object))

(define (add-classes cls env)
  (if (empty? cls) env
  (add-classes (cdr cls) (value-of-class (car cls) env))))

;(define x1 '('prog '(classes '(class a extends object '(a b c) '('('proc '() ) ))))
;  )