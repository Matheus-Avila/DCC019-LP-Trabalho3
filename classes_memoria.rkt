#lang racket

;Matheus Avila Moreira de Paula 201565191C
;Leonardo Azalim de Oliveira 201965251C

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

; call-by-reference
(define (proc-val var exp Δ)
  (lambda (val flag)
    (if flag (value-of exp (extend-env var (newref val) Δ))
        (value-of exp (extend-env var val Δ)))))

(define (apply-proc proc val)
  (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))

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
        [(equal? type 'new) (new-instance (cadr exp) Δ (get-addr-free))]
        [(equal? type 'set-val) (set-field (cadr exp) (caddr exp) Δ)]
        [(equal? type 'send) (send-field (cadr exp) Δ)]
        [(equal? type 'display) (display (cadr exp))]
        
        [else (error "operação não implementada")]))

;Definição da semantica de classes
;'(classes '(class A extends object '(Fields) '(Methods)) '(class B extends A '(Fields) '(Methods)))

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

(define (value-of-fields cls pai env)
  (if (empty? cls) '()
      (extend-env-class (car cls) 0 (value-of-fields (cdr cls) pai env)) ))

;Coloca cada valor de fields com valor nulo
(define (value-of-definition cls pai env)
(if (empty? cls) env
  (extend-env-class 'fields (value-of-fields (car cls) pai env) env))
  )

;'(class A extends object ... )
;Associa '(A object) com Fields e Methods
(define (value-of-class cls env)
  (if (and (equal? (car cls) 'class) (equal? (caddr cls) 'extends))
      (extend-env-class (list (cadr cls) (cadddr cls)) (value-of-definition (cddddr cls) (cadddr cls) env) env)
      (error "Erro nas palavras chave classe ou extends"))
  )

;'(classes '(A) '(B))
; Add a classe A no env em que Add B no env vazio e termina incluindo a classe object
(define (add-classes cls env)
  (if (empty? cls) env
  (list (value-of-class (car cls) env) (add-classes (cdr cls) env) '(object))))

;itc = classe
;(caaar env) = Nome da classe que estou olhando agora
;(cdar env) = Lista com os Fields da classe
; (cadar env) = (fields (a 0 (b 0 (c 0 ()))) ())
(define (new-instance obj env addr-free)
  (if (empty? env) (error "Classe inexistente!\nNao foi possivel instanciar o objeto")
      (if (equal? obj (caaar env))
          (begin (insert-in-memory (cadr (cadar env))) (return-instance (cadr (cadar env)) addr-free))
          (new-instance obj (cadr env) addr-free))
  )
)

;Retorna para o corpo principal da funcao uma lista com o inicio da instancia e o mapeamento da pos de cada campo
(define (return-instance cls addr-free)
  (if (empty? cls) '()
      (list (car cls) addr-free (return-instance (caddr cls) (+ addr-free 1)))
  )
)

;Insere os campos na memoria
(define (insert-in-memory cls)
  (if (empty? cls) (println "Instancia criada com sucesso!")
      (begin (newref (cadr cls)) (insert-in-memory (caddr cls)) )    
    )
 )
;Define o valor do atributo fld para val na instancia obj
(define (set-field fld val obj)
  (if (empty? obj) (error "Valor nao existe nessa instancia")
      (if (equal? fld (car obj)) (setref! (cadr obj) val)
          (set-field fld val (caddr obj))
      )
  )
)
;Procura o campo fld no objeto cls
(define (send-field fld cls)
  (if (empty? cls) (error "O campo escolhido nao é uma variável nem um método")
      (if (equal? fld (car cls)) (deref (cadr cls))
          (send-field fld (caddr cls))
      )
  )
)
; Exemplos de execucao
; Exemplo 1
(define exemploDisplay '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (display "\nFim do primeiro exemplo\n") )))
(display "\n- Exemplo exemploDisplay:\n") (value-of exemploDisplay empty-env-class)
; Exemplo 2
(define exemploDeErroClasseNaoDeclarada '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classeNaoDeclarada) (set-val a 2 c1) )))
(display "\nFavor descomentar a linha abaixo para executar o exemploDeErroClasseNaoDeclarada\n")
;(display "\n- Exemplo exemploDeErroClasseNaoDeclarada:\n") (value-of exemploDeErroClasseNaoDeclarada empty-env-class)
; Exemplo 3
(define mudarCampo '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (set-val a 2 c1) )))
(display "\n- Exemplo mudarCampo:\n") (value-of mudarCampo empty-env-class)
; Exemplo 4
(define pegaCampo '(main_prog (classes (class classe1 extends classe2 (a b c)) (class classe2 extends object (d e f))) (let c1 (new classe1) (begin (set-val a 5 c1) (display "\nValor de a na intancia c1: ") (send a c1) ))))
(display "\n- Exemplo pegaCampo:\n") (value-of pegaCampo empty-env-class)
; Exemplo 5
(define criacaoDeClasse '(classes (class classe1 extends object (a b c))))
(display "\n- Exemplo criacaoDeClasse:\n") (value-of criacaoDeClasse empty-env-class)
; Exemplo 6
(define subtraiCampos
  '(main_prog (classes (class classe1 extends classe2 (a b c))
                       (class classe2 extends object (d e f)))
              (let c1 (new classe1)
                (begin (set-val a 5 c1) (set-val b 1 c1)
                       (display "\nDiferença entre os valores 'a' e 'b' na intancia c1: ")
                       (dif (send a c1) (send b c1))
                       ))))
(display "\n- Exemplo subtraiCampos:\n") (value-of subtraiCampos empty-env-class)
; Exemplo 7
(define visualizaObjeto
  '(main_prog (classes (class classe1 extends classe2 (a b c))
                       (class classe2 extends object (d e f)))
              (begin (new classe1) (new classe2)) ))
(display "\n- Exemplo visualizaObjeto:\n") (value-of visualizaObjeto empty-env-class)