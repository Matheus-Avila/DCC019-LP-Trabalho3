#lang racket

#| *** Definição do Environment *** |#

(define empty-env
  (lambda (var)
    (error "No bind")))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (proc-val var body (extend-env-rec name var body env))
        (apply-env env svar))))

(define (apply-env env var)
  (env var))

(define init-env empty-env)

#| *** Representação de procedimentos *** |#

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var val Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))


#|
; A Linguagem INFERRED

; Sintaxe
Prog -> (prog Expr)
Expr -> (lit Val) | (var Var) | (zero? Expr) | (dif Expr Expr)
     |  (if Expr Expr Expr) | (let Var Expr Expr)
     | (proc Var Expr) | (call Expr Expr)
     | (letrec Name Var Expr Expr)


; Representação para variáveis de tipos

Type -> int | bool | (proc-type Type Type)
       | (tyvar Num)

* Exemplo:

proc (f) proc (x) -((f 3), (f x))

           Expressão                          Variável de Tipo
____________________________________________________________________
              f                                (tvar-type 0)
              x                                (tvar-type 1)
proc (f) proc (x) -((f 3), (f x))              (tvar-type 2)
    proc (x) -((f 3), (f x))                   (tvar-type 3)
       -((f 3), (f x))                         (tvar-type 4)
            (f 3)                              (tvar-type 5)
            (f x)                              (tvar-type 6)

; Substituição

    Solução parcial                    Incluir
 _____________________            _________________
  t₂ = t₀ → (t₁ → int)                 t₄ = int

 * Representação
   - par: variável de tipo X tipo

; apply-subst :: Type x TyVar x Type -> Type

|#

; tyvar? :: Type -> Bool 
(define (tyvar? ty)
  (and (list? ty) (equal? (car ty) 'tyvar)))

; proc-type? :: Type -> Bool
(define (proc-type? ty)
  (and (list? ty) (equal? (car ty) 'proc-type)))

#| *** Representação das Substituições ***|#
; * Uma substituição é uma lista pares: (TyVar . Type)

; search-subst :: TyVar x Subst -> (TyVar . Type)
(define (search-subst ty sub)
  (if (empty? sub) #f
      (if (equal? (cadr ty) (cadr (car (first sub)))) (first sub)
          (search-subst ty (rest sub)))))

; apply-subst :: Type x TyVar x Type -> Type
(define (apply-subst t₁ vt t₂)
  (cond [(equal? t₁ 'int) 'int]
        [(equal? t₁ 'bool) 'bool]
        [(proc-type? t₁) (list 'proc-type (apply-subst (cadr t₁) vt t₂)
                                                             (apply-subst (caddr t₁) vt t₂))]
        [(tyvar? t₁) (if (equal? (cadr t₁) (cadr vt)) t₂ t₁)]
        [else (error "Expressão não representa um tipo")]))

; Exemplo de uso da função applu-subst
(apply-subst '(proc-type (tyvar 1) (proc-type (tyvar 0) int)) '(tyvar 0) 'bool)        

; apply-subst-type :: Type x Subs -> Type
#;(define (apply-subst-type type sub)
  (foldl (lambda (s ty)
           (apply-subst ty (car s) (cdr s))) type sub))

(define (apply-subst-type type sub)
  (cond [(equal? type 'int) 'int]
        [(equal? type 'bool) 'bool]
        [(proc-type? type) (list 'proc-type (apply-subst-type (cadr type) sub)
                                                                 (apply-subst-type (caddr type) sub))]
        [(tyvar? type) (let [(tmp (search-subst type sub))]
                                                         (if tmp (cdr tmp) type))]
        [else (error "Expressão não representa um tipo")]))
                                                             
; Exemplo e uso da função apply-subst-type
(apply-subst-type '(proc-type (tyvar 0) (tyvar 1)) (list (cons '(tyvar 0) 'int) (cons '(tyvar 1) 'bool)))


; empty-subst :: Subst
(define empty-subst null)

; extend-subst :: Subst x TyVar x Type -> Subst
(define (extend-subst sub var ty)
  (cons (cons var ty)
        (map (lambda (s)
               (cons (car s)
                     (apply-subst (cdr s) var ty))) sub)))


#| *** Unificador *** |#
#|

Expressões                             Tipo               Equações                Substituição
__________________________________   _________   ________________________       ________________
f                                        t₀             t₂ = t₀ → t₃              t₂ = t₀ → t₃
x                                        t₁             t₃ = t₁ → t₄
proc (f) proc (x) -((f 3), (f x))        t₂             t₅ = int
proc (x) -((f 3), (f x))                 t₃             t₆ = int
-((f 3), (f x))                          t₄             t₄ = int
(f 3)                                    t₅             t₀ = int → t₅
(f x)                                    t₆             t₀ = t₁ → t₆

|#

; no-occorrence? :: TyVar -> Bool
(define (no-occurrence? tvar t₂)
  (cond [(equal? t₂ 'int) #t]
        [(equal? t₂ 'bool) #t]
        [(proc-type? t₂) (and (no-occurrence? tvar (cadr t₂))
                              (no-occurrence? tvar (caddr t₂)))]
        [(tyvar? t₂) (not (equal? tvar t₂))]))


; unifier :: Type x Type x Subst -> Subst
(define (unifier t₁ t₂ sub)
  ; 1º aplicamos a substituição em ambos os tipos
  (let ([ty₁ (apply-subst-type t₁ sub)]
        [ty₂ (apply-subst-type t₂ sub)])
    (cond [(equal? ty₁ ty₂) sub] ; tipos iguais, remove a equação
          ; Lado esquerdo é uma variável e não ocorre no lado direito
          [(tyvar? ty₁) (if (no-occurrence? ty₁ ty₂) (extend-subst sub ty₁ ty₂)
                            (error "Não unifica: teste de ocorrência"))]
          ; Lado direito é uma variável e não ocorre no lado esquerdo
          [(tyvar? ty₂) (if (no-occurrence? ty₂ ty₁) (extend-subst sub ty₂ ty₁)
                            (error "Não unifica: teste de ocorrência"))]
          ; Nenhum dos dois lados é uma variável
          [(and (proc-type? ty₁) (proc-type? ty₂))
           (let [(sub₁ (unifier (cadr ty₁) (cadr ty₂) sub))] ; unifica os argumentos
             (let [(sub₂ (unifier (caddr ty₁) (caddr ty₂) sub₁))] ; unifica os retornos
               sub₂))]
          [else (error "Impossível unificar")])))

(define number 0)

(define (new-tyvar)
  (define num number)
  (set! number (add1 number))
  (list 'tyvar num))

; type-of :: Expr x Env x Subst -> (Type . Subst)
(define (type-of exp env sub)
  (let [(type (car exp))] ; pega o tipo da expressão
    (cond [(equal? type 'lit) (if (number? (cadr exp)) (cons 'int sub) (cons 'bool sub))]
          [(equal? type 'var) (cons (apply-env env (cadr exp)) sub)]
          [(equal? type 'dif) (let [(r₁ (type-of (cadr exp) env sub))]
                                (let [(sub₁ (unifier (car r₁) 'int (cdr r₁)))]
                                  (let [(r₂ (type-of (caddr exp) env sub₁))]
                                    (let [(sub₂ (unifier (car r₂) 'int (cdr r₂)))]
                                      (cons 'int sub₂)))))]

          [(equal? type 'zero?) (let [(t (type-of (cadr exp) env sub))]
                                  (let [(sub₂ (unifier (car t) 'int (cdr t)))]
                                    (cons 'bool sub₂)))]
          
          [(equal? type 'if) (let [(r₁ (type-of (cadr exp) env sub))]
                               (let [(sub₁ (unifier (car r₁) 'bool (cdr r₁)))]
                                 (let [(r₂ (type-of (caddr exp) env sub₁))]
                                   (let [(r₃ (type-of (cadddr exp) env (cdr r₂)))]
                                     (let [(sub₂ (unifier (car r₂) (car r₃) (cdr r₃)))]
                                       (cons (car r₂) sub₂))))))]

          [(equal? type 'call) (let [(result-type (new-tyvar))
                                     (r₁ (type-of (cadr exp) env sub))]
                                 (let [(r₂ (type-of (caddr exp) env (cdr r₁)))]
                                   (let [(sub₁ (unifier (car r₁) (list 'proc-type (car r₂) result-type) (cdr r₂)))]
                                     (cons result-type sub₁))))]

          [(equal? type 'proc) (let* [(vtype (new-tyvar))
                                     (r₁ (type-of (caddr exp) (extend-env (cadr exp) vtype env) sub))]
                                 (cons (list 'proc-type vtype (car r₁)) (cdr r₁)))]

          [(equal? type 'let) (error "Exercício")]
          [(equal? type 'letrec) (error "Exercício")]
          [else (error "Expressão não implementada")])))

;(type-of '(proc x (var x)) init-env empty-subst)


; Especificação do comportamento das expressões
(define (value-of exp Δ)
  (let [(type (car exp))]
    (cond [(equal? type 'lit) (cadr exp)]
          [(equal? type 'var) (apply-env Δ (cadr exp))]
          [(equal? type 'dif) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
          [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]
          [(equal? type 'let) (value-of (cadddr exp) (extend-env (cadr exp) (value-of (caddr exp) Δ)  Δ))]
          [(equal? type 'if) (if (value-of (cadr exp) Δ)
                                 (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]
          
          [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ)]
          [(equal? type 'call) (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
          [(equal? type 'letrec) (let* [(fun-name (cadr exp))
                                       (fun-body (cadddr exp))
                                       (param-name (caddr exp))
                                       (body (car (cddddr exp)))]
                                   (value-of body (extend-env-rec fun-name param-name fun-body Δ)))]
          [else (error "operação não implementada")])))

; Especificação do comportamento de programas
(define (value-of-program prog)
  (type-of (cadr prog) init-env empty-subst)
  (value-of (cadr prog) init-env))

; Exemplos de programas
(define ex1 '(proc f (proc x (dif (call (var f) (lit 3)) (call (var f) (var x))))))
(value-of-program `(prog ,ex1))

;(define ex1 '(call (proc x (var x)) (lit 3)))
;(value-of-program `(prog ,ex1))

(define ex2 '(letrec double x (if (zero? (var x))
                                          (lit 0)
                                          (dif (call (var double) (dif (var x) (lit 1))) (lit -2)))
                     (call (var double) (lit 3))))
;(value-of-program `(prog ,ex2))
