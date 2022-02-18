#lang racket

; Representando um estado como um par: próximo endereço e um vetor
(define TAM 1000) ; tamanho da memória
(define σ (cons 0 (make-vector TAM)))

;empty-store
(define (empty-store) (set! σ (cons 0 (cdr σ))))

;newref :: ExpVal -> Ref
(define (newref v)
  (define addr (car σ))
  (define mem (cdr σ))
  (vector-set! mem addr v)
  (set! σ (cons (add1 addr) mem))
  addr)

; deref :: Ref -> ExpVal
(define (deref addr)
  (if (< addr (car σ))
         (vector-ref (cdr σ) addr)
         (error "invalid location")))
; setref! :: Ref x ExpVal -> ?
(define (setref! addr v)
  (if (< addr (car σ))
      (vector-set! (cdr σ) addr v)
      (error "invalid location")))
;Retorna onde vai começar meu novo objeto
(define (get-addr-free)
  (car σ))

;Matheus Avila Moreira de Paula 201565191C
;Leonardo Azalim de Oliveira 201965251C

;Definicao do ambiente na forma de lista
(define empty-env '(empty-env))

(define (extend-env var value env) (list 'extend-env var value env))
(define (apply-env env var) (if (equal? 'empty-env (car env)) #f (if (equal? var (cadr env)) (caddr env) (apply-env (cadddr env) var))))


(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

(define init-env empty-env)

; call-by-value
; proc-val :: Var x Expr x Env -> Proc
#;(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
#;(define (apply-proc proc val)
  (proc val))


; call-by-reference
(define (proc-val var exp Δ)
  (lambda (val flag)
    (if flag (value-of exp (extend-env var (newref val) Δ))
        (value-of exp (extend-env var val Δ)))))

(define (apply-proc proc val)
  (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))

(struct thunk (env exp))

(define p1 '(let p (proc x (set x (lit 4)))
                (let a (lit 3)
                  (begin (call (var p) (var a)) (var a)))))

(define p2 '(let f (proc x (set x (lit 44)))
              (let g (proc y (call (var f) (var y)))
                (let z (lit 55)
                  (begin (call (var g) (var z)) (var z))))))

(define p3 '(let swap (proc x (proc y (let temp (var x)
                                        (begin (set x (var y))
                                               (set y (var temp))))))
              (let a (lit 33)
                (let b (lit 44)
                  (begin (call (call (var swap) (var a)) (var b))
                         (dif (var a) (var b)))))))

(define p4 '(letrec loop x (call (var loop) (dif (var x) (lit -1)))
                    (let f (proc x (lit 7))
                      (call (var f) (call (var loop) (lit 0))))))

(define (value-of exp Δ)
  (define type (car exp))
  (cond [(equal? type 'lit) (cadr exp)]
        ; call-by-value e call-by-reference
        ;[(equal? type 'var) (deref (apply-env Δ (cadr exp)))]
        ; call-by-name
        [(equal? type 'var) (define v (cadr exp))
                            (if (thunk? v) (value-of (thunk-exp v) (thunk-env v))
                                (deref (apply-env Δ v)))]
        [(equal? type 'dif) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]
        [(equal? type 'let) (value-of (cadddr exp) (extend-env (cadr exp) (newref (value-of (caddr exp) Δ)) Δ))]
        [(equal? type 'if) (if (value-of (cadr exp) Δ)
                               (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]
        [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ)]
        ; call-by-value
        ;[(equal? type 'call) (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        ; call-by-reference
        #;[(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ)))]
        ;call-by-name
        [(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (thunk Δ (caddr exp))))]
        
        [(equal? type 'letrec) (value-of (car (cddddr exp)) (extend-env-rec (cadr exp) (caddr exp) (cadddr exp) Δ))]

        [(equal? type 'set) (let ([v (value-of (caddr exp) Δ)])
                              (setref! (apply-env Δ (cadr exp)) v)
                              v)]
        
        [(equal? type 'begin) (foldl (lambda (e acc)
                                       (value-of e Δ))
                                     (value-of (cadr exp) Δ)
                                     (cddr exp))]
        [(equal? type 'classes) (add-classes (cdr exp) Δ)]
        [(equal? type 'main_prog) (value-of (caddr exp) (value-of (cadr exp) Δ))]
        [(equal? type 'new) (new-instance (cadr exp) Δ)]
        [(equal? type 'set-val) (set-field (cadr exp) (caddr exp) Δ)]
        [(equal? type 'send) (send (cadr exp) (caddr exp) Δ)]
        
        [else (error "operação não implementada")]))


; Especificação do comportamento de programas
(define (value-of-program prog)
  (empty-store)
  (value-of (cadr prog) init-env))

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
  (if (equal? itc (caaar env)) (insert-in-memory (cdar env) (get-addr-free)) (new-instance itc (cdr env)))
  )
)

;Se der errado tentar fazer usando uma variável para guardar a lista
(define
(insert-in-memory cls start)
(if (empty? cls) (display "Instancia criada com sucesso!")
    (begin (newref (cadr cls)) (list (car cls) start (insert-in-memory (cddr cls) (+ start 1)) )
           )
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

; Exemplos de expressões IREF
#;(define ex1 '(let g (let count (lit 0)
                      (proc dummy (begin (set count (dif (var count) (lit -1)))
                                         (var count))))
               (let a (call (var g) (lit 11))
                 (let b (call (var g) (lit 11))
                   (dif (var a) (var b))))))

;(value-of ex1 init-env)

#;(define ex2 '(program
              (letrec fun x (if (zero? (var x)) (lit 0)
                                (dif (var x)
                                     (dif (lit 0)
                                          (call (var fun) (dif (var x) (lit 1))))))
                      (call (var fun) (lit 3)))))

;(value-of-program ex2)


;(value-of p1 init-env)
;(value-of p2 init-env)
;(value-of p3 init-env)
;(value-of p4 init-env)


(define exemploDisplay '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (display "o objeto foi criado!") )))
;(value-of exemploDisplay empty-env-class)

(define exemploDeErro '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classeNaoDeclarada) (set-val a 2 c1) )))
;(value-of exemploDeErro empty-env-class)

(define mudarCampo '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (set-val a 2 c1) )))
;(value-of mudarCampo empty-env-class)

(define criacaoDeClasse '(classes (class classe1 extends object (a b c))))
;(value-of criacaoDeClasse empty-env-class)
