#lang racket

#| 
Linguagem PROC

; Sintaxe
Prog -> (prog Expr)
Expr -> (lit Val) | (var Var) | (zero? Expr) | (dif Expr Expr)
     |  (if Expr Expr Expr) | (let Var Expr Expr)
     | (proc Var Expr) | (call Expr Expr)

; Examples

1) let f = proc (x) -(x,11) in (f (f 77))
__________
(let f (proc x (dif (var x) (lit 11)))
  (call (var f) (call (var f) (lit 77))


2) (proc (f) (f (f 77))
    proc (x) -(x,11))
____________
(call (proc f (call (var f) (call (var f) (lit 77))))
      (proc x (dif (var x) (lit 11))))


; Valores e interface
  ; Valores das expressões (Expressed values)
    ExpVal = Int + Bool + Proc
  ; Valores denotados (Denoted values)
    DenVal = Int + Bool + Proc

  ; Interface
    Usar os número inteiros e boolean de Racket

    num-val :: Int -> ExpVal
    bool-val :: Bool -> ExpVal
    expval->num :: ExpVal -> Int
    expval->bool :: ExpVal -> Bool

; Considere o seguinte exemplo:
let x = 200
in let f = proc (z) -(z,x)   <====== Quanto vale x?
   in let x = 100
      in let g = proc (z) -(z,x) <====== Quanto vale x?
         in -((f 1), (g 1))

; Escopo estático ou escopo léxico 

    proc-val :: Var x Expr x Env -> Proc
    apply-proc :: Proc x ExpVal -> ExpVal

; Environment
Env = Var -> Value

empty-env :: Env
extend-env :: Var x Value x Env -> Env
apply-env :: Env x Var -> Value
    
; Notação
  Δ => Environment
  [] => empty-env
  [var=val]Δ => (extend-env var val Δ)
  [var1=val1, var2=val2]Δ => abreviação para [var1=val1]([var2=val2]Δ)
  [var1=val1, var2=val2, ..., varn=valn] => abreviação para [var1=val1,...,varn=valn][]
  Δ⟦var⟧  => (apply-env Δ var)
|#

; Definição do Environment
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

; Representação de procedimentos

#|   
var =>  Variável
body => Expr
env => Environment de execução (escopo estático)
val => ExpVal

(apply-proc (procedure var body env) val
  (value-of body (extend-env var val env))
|#

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


#| Semântica de PROC

        [] ⊢ e = v
0)  _____________________
        (prog e) = ?

1)  __________________
     Δ ⊢ (lit n) = n

2) _____________________
    Δ ⊢ (var v) = Δ⟦v⟧

       Δ ⊢ e = 0                   Δ ⊢ e ≠ 0
3) _____________________      ______________________
    Δ ⊢ (zero? e) = #t         Δ ⊢ (zero? e) = #f 


       Δ ⊢ e₁ = v₁ Δ ⊢ e₂ = v₂
4)  ______________________________
      Δ ⊢ (dif e₁ e₂) = v₁ - v₂

       Δ ⊢ e₁ = #t Δ ⊢ e₂ = v            Δ ⊢ e₁ = #f Δ ⊢ e₃ = v
5)   __________________________          ___________________________
       Δ ⊢ (if e₁ e₂ e₃) = v               Δ ⊢ (if e₁ e₂ e₃) = v

      Δ ⊢ e₁ = v    [var = v]Δ ⊢ e₂ = v₂
6)  _______________________________________
          Δ ⊢ (let var e₁ e₂) = v₂

; Escopo Estático

7) _______________________________________________
    Δ ⊢ (proc var body) = (proc-val var body Δ)

             Δ ⊢ e₁ = p     Δ ⊢ e₂ = v
8) _______________________________________________
    Δ ⊢ (call e₁ e₂) = (apply-proc p v)

let x = 200 [x=100]
in let f = proc (z) -(z,x) [f=procedure(z -(z,x)), x=100]
   in let x = 100 [x=100, f=procedure(z -(z,x)), x=100]
      in let g = proc (z) -(z,x) [g=procedure(z -(z,x)), x=100, f=procedure(z -(z,x)), x=100]
         in -((f 1), (g 1)) ===> (1 - 100) - (1 -100) = 0

; Escopo Dinâmico

7) _______________________________________________
    Δ ⊢ (proc var body) = (proc-val var body)

             Δ ⊢ e₁ = p     Δ ⊢ e₂ = v
8) _______________________________________________
    Δ ⊢ (call e₁ e₂) = (apply-proc p v Δ)

 
|#

; Especificação do comportamento das expressões
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
        ;[(equal? type 'proc) (proc-val (cadr exp) (caddr exp))]
        ;[(equal? type 'call) (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ) Δ)]
        [else (error "operação não implementada")]))

; Especificação do comportamento de programas
(define (value-of-program prog)
  (value-of (cadr prog) init-env))

; Exemplos de expressões PROC
(define ex1 '(let f (proc x (dif (var x) (lit 11)))
  (call (var f) (call (var f) (lit 77)))))

(value-of ex1 init-env)

(define ex2 '(call (proc f (call (var f)
                    (call (var f) (lit 77))))
      (proc x (dif (var x) (lit 11)))))

(value-of ex2 init-env)

(define ex3 '(let x (lit 200)
               (let f (proc z (dif (var z) (var x)))
                 (let x (lit 100)
                   (let g (proc z (dif (var z) (var x)))
                     (dif (call (var f) (lit 1))
                          (call (var g) (lit 1))))))))
                   
(value-of ex3 init-env)

(define q1 '(let x (lit 7)
              (let y (lit 2)
                (let y (let x (dif (var x) (lit 1))
                         (dif (var x) (var y)))
                  (dif (dif (var x) (lit 8)) (var y))))))

(value-of q1 init-env)
                                   


(define makemult '(let makemult
                      (proc maker
                            (proc x (if (zero? (var x)) (lit 0)
                                        (dif (call (call (var maker) (var maker))
                                                   (dif (var x) (lit 1)))
                                             (lit -4)))))
                    (let times4 (proc x (call (call (var makemult) (var makemult)) (var x)))
                      (call (var times4) (lit 3)))))

(value-of makemult init-env)











