#lang racket
;Matheus Avila Moreira de Paula 201565191C
;Leonardo Azalim de Oliveira 201965251C

;Definicao do ambiente na forma de lista
(define empty-env '(empty-env))

(define (extend-env var value env) (list 'extend-env var value env))
(define (apply-env env var) (if (equal? 'empty-env (car env)) #f (if (equal? var (cadr env)) (caddr env) (apply-env (cadddr env) var))))

;Definicao procedural do professor
;(define empty-env  (lambda (var)    (error "No bind")))

;(define (extend-env var value env)  (lambda (svar)    (if (equal? svar var) value        (apply-env env svar))))

;(define (apply-env env var)   (env var))

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
;(define (proc-val var exp)
;  (lambda (val Δ)
;    (value-of exp (extend-env var val Δ))))

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
        [(equal? type 'main_prog) (value-of (caddr exp) (value-of (cadr exp) Δ))]
        [(equal? type 'new) (new-instance (cadr exp) Δ)]
        [(equal? type 'set-val) (set-field (cadr exp) (caddr exp) Δ)]
        [(equal? type 'send) (send (cadr exp) (caddr exp) Δ)]
        [(equal? type 'display) (display (cadr exp))]
        [else (error type)]))

; Especificação do comportamento de programas
(define (value-of-program prog)
  (value-of (cadr prog) init-env))

;Comecei a escrever daqui pra baixo vvvvvvvvvvvvvvv
;Definição da semantica de classes
;'(classes '(class A extends object '(Fields) '(Methods)) '(class B extends A '(Fields) '(Methods)))

(define (send cls fld)
  (if (empty? cls) (error "O campo escolhido nao é uma variável nem um método")
      (if (equal? fld (car cls))
          (cadr cls)
          (send (cddr cls) fld)
      )
  )
)

(define (get-pai nome env)
(if (empty? env)
  (error "A classe pai não está definida!")
  (if (equal? nome (car env))
      (cadr env)
      (get-pai nome (cddr env))
   )
 )
)

(define empty-env-class empty)

(define (extend-env-class var value env)
  (list var value env)
  )

(define (apply-env-class env var)
  (if (equal? 'empty-env-class (car env)) (error "Env vazio!")
      (if (equal? var (cadr env)) (caddr env)
          (apply-env-class (cadddr env) var))
      )
  )

;Funcao backup. Remover se a função que trata fields e methods como um so der certo
(define (value-of-fields cls pai env)
(if (empty? cls) '()
    (extend-env-class (car cls) '(lit 0) (value-of-fields (cdr cls) pai env));trocar lit 0 por 0 se der errado
    ))


;Se o 2 elemento for uma lista que comeca com proc extend-env-class o field com o value of do proc. CC faz o que já tá escrito
(define (value-of-fields2 cls pai env)
(if (empty? cls)
    '()
    (if (and (list? (cadr cls)) (equal? (caadr cls) 'proc))
        (extend-env-class (car cls) (value-of (cadr cls) env) (value-of-fields (cddr cls) pai env) )
        (if (and (list? (cadr cls)) (equal? (caadr cls) 'super))
            ((extend-env-class (car cls) (value-of (cadr cls) (get-pai pai env)) (value-of-fields (cddr cls) pai env) ))
            (extend-env-class (car cls) '(lit 0) (value-of-fields (cdr cls) pai env));trocar lit 0 por 0 se der errado
        )
    )
  )
)

; Essa funcao provavelmente sera descartada
(define (value-of-methods cls env)
(if (empty? cls) '()
    (extend-env-class (car cls) (value-of (list 'proc (cadr cls)) env) (value-of-fields (cdr cls) env))
    )
  )

;Coloca cada valor de fields com valor nulo
(define (value-of-definition cls pai env)
(if (empty? cls) env
  (extend-env-class 'fields (value-of-fields (car cls) pai env) env))
  )

;'(class A extends object ... )
;Associa '(A object) com Fields e Methods
(define (value-of-class cls env)
 (extend-env-class (list (cadr cls) (cadddr cls)) (value-of-definition (cddddr cls) (cadddr cls) env) env)
  )

;'(classes '(A) '(B))
; Add a classe A no env em que Add B no env vazio('(object))

(define (add-classes cls env)
  (if (empty? cls) env
  (list (value-of-class (car cls) env) (add-classes (cdr cls) env) '(object))))

;itc = classe
;(caaar env) = Nome da classe que estou olhando agora
;(cdar env) = Lista com os Fields da classe
(define
(new-instance itc env)
  (if (equal? env '((object))) (error "Nao existe essa classe!!")
  (if (equal? itc (caaar env)) (cdar env) (new-instance itc (cdr env)))
  )
)

;set
(define
  (set-field fld val env)
  (if (empty? env) (error "Valor nao existe nessa instancia")
  (if (equal? fld (car env)) (list fld val (caddr env)) ;"troca o valor associado a fld para val"
   (set-field fld val (caddr env)) )
  )
  )

;(define x1 '(classes (class classe1 extends object (a b c) ))
;  )
(define exemploDisplay '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (display "o objeto foi criado!") )))
;(value-of exemploDisplay empty-env-class)

(define exemploDeErro '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classeNaoDeclarada) (set-val a 2 c1) )))
;(value-of exemploDeErro empty-env-class)

(define mudarCampo '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (set-val a 2 c1) )))
;(value-of mudarCampo empty-env-class)

(define criacaoDeClasse '(classes (class classe1 extends object (a b c))))
(value-of criacaoDeClasse empty-env-class)

(define criacaoMetodos '(classes (class classe1 extends object (a b c d (proc x (dif (var x) (lit 5)))))))
;(value-of criacaoMetodos empty-env-class)
