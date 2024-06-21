#lang eopl
(require racket/vector)
(require "complemento.rkt")
(provide (all-defined-out))

(define lexica
  '((white-sp
     (whitespace) skip)
    (comment
     ("//" (arbno (not #\newline))) skip)
    (identificador
     (letter (arbno (or letter digit "?"))) symbol)
    (digitoBinario
     ("b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoBinario
     ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoDecimal
     (digit (arbno digit)) number)
    (digitoDecimal
     ("-" digit (arbno digit)) number)
    (digitoOctal
     ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoOctal
     ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoHexadecimal
     ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
    (digitoHexadecimal
     ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
    (flotante
     (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)
    (expresion ("void") void-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)

    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)

    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)

    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)

    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)

    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)

    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)

    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (structs  exp) (
        cond 
        [(or (null? struct-exp) (eqv? '() structs)) (eval-expresion exp ambiente-inicial)]
        [else (eval-expresion exp (extend-env (map (lambda(m)(car m)) (map (lambda (struc)(eval-struct-decl struc) )structs)) (map (lambda(m)(cadr m)) (map (lambda (struc)(eval-struct-decl struc) )structs)) ambiente-inicial) )]
        )
      )
    )
  )
)

;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "my-language: --> "
                        eval-program(sllgen:make-stream-parser
                                     lexica
                                     gramatica)))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?))
)

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define expval?
  (lambda (x)
    (or (number? x) (boolean? x) (string? x) (vector? x) (symbol? x) (list? x) (procval? x))))

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define ambiente-inicial (empty-env))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (numero) (eval-numero-exp numero))
      (void-exp () 'void-exp)
      (var-exp (id) (apply-env env id))
      (bool-exp (bool) (eval-bool-exp bool))
      (prim-num-exp (exp1 prim exp2) (aplicar-primitiva prim (eval-expresion exp1 env) (eval-expresion exp2 env)))
      (prim-bool-exp (prim exps) (aplicar-primitiva-bool prim (map (lambda (id) (eval-expresion id env)) exps)))
      (cadena-exp (text ltexts) (cadena-expression (symbol->string text) (map (lambda (lt) (symbol->string lt)) ltexts)))
      (prim-cad-exp (prim exps) (aplicar-primitiva-cadena prim (map (lambda (str) (eval-expresion str env)) exps)))
      (decl-exp (decl) (eval-decl-exp decl env))
      (lista-exp (exps) (map (lambda (item) (eval-expresion item env)) exps))
      (prim-list-exp (prim exp) (aplicar-primitiva-listas prim (eval-expresion exp env)))
      (empty-list-exp () '())
      (cons-exp (exp1 exp2) (cons (eval-expresion exp1 env)(eval-expresion exp2 env)))
      (array-exp (exps) (list->vector (map (lambda (item) (eval-expresion item env)) exps)))
      (prim-array-exp (prim exps) (aplicar-primitiva-array prim (map (lambda (item) (eval-expresion item env)) exps)))
      (if-exp (condicion then elses) (if (eval-expresion condicion env) (eval-expresion then env) (eval-expresion elses env)))
      (set-exp (id rhs-exp) (begin (setref! (apply-env-ref env id) (eval-expresion rhs-exp env)) (eval-expresion (void-exp) env)))
      (begin-exp (exp exps) (let loop ((acc (eval-expresion exp env)) (exps exps)) (if (null? exps) acc (loop (eval-expresion (car exps) env) (cdr exps)))))
      
      (for-exp (itdor from until by body) (eval-for-exp itdor from until by body (extend-env (list itdor) (list (eval-var-exp-rand from env)) env)))
      
      (while-exp (condicion cuerpo) (apply-while-exp condicion cuerpo env))
      (switch-exp (item cases caseValues defaultValue) (eval-switch-exp item  (map (lambda (caseKey) (eval-expresion caseKey env)) cases)
      (map (lambda (caseValue) (eval-expresion caseValue env)) caseValues) (eval-expresion defaultValue env) env))

      (func-exp (ids body) (closure ids body env))


      (call-exp (rator rands)(let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (match-exp (item regulars-exp casesValue)(eval-match-exp item regulars-exp casesValue env)) 
      (new-struct-exp (id rands) (list id (map (lambda (x) (eval-expresion x env)) rands)))
      (get-struct-exp (id exp) (extractItem (searchItem exp (primitive-deref (apply-env-ref env (car (eval-expresion id env))))) (cadr (eval-expresion id env))))
      (set-struct-exp (id exp1 exp2) (begin (setref! (apply-env-ref env (extract-id id))
      (list (car (eval-expresion id env))(replace-in-list (cadr (eval-expresion id env)) 
        (searchItem exp1 (primitive-deref (apply-env-ref env (car (eval-expresion id env))))) 
        (eval-expresion exp2 env)))) (eval-expresion (void-exp) env)))
      )
    )
)

(define eval-match-exp
  (lambda (item cases-regular-exp valuesItem env)
       (let ((regular (car cases-regular-exp)))
              (cases regular-exp regular
                (num-match-exp (id) (if (number? (eval-expresion item env)) (eval-expresion (car valuesItem) (extend-env (list id) (list (direct-target (eval-expresion item env)))env)) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                (cad-match-exp (id) (if (string? (eval-expresion item env)) (eval-expresion (car valuesItem) (extend-env (list id) (list (direct-target (eval-expresion item env)))env)) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                (bool-match-exp (id) (if (boolean? (eval-expresion item env)) (eval-expresion (car valuesItem) (extend-env (list id) (list (direct-target (eval-expresion item env)))env)) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                (array-match-exp (ids) (if (vector? (eval-expresion item env)) (eval-expresion (car valuesItem) (extend-env (list (car ids)) (list (direct-target (car (vector->list(vector-copy (eval-expresion item env) 0 1))))) (extend-env (list (cadr ids)) (list (direct-target (car (vector->list (vector-copy (eval-expresion item env) 1 2))))) (extend-env (list (caddr ids)) (list (direct-target (vector-copy (eval-expresion item env) 2 (vector-length (eval-expresion item env))))) env)))) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                (empty-match-exp () (if (null? (eval-expresion item env ))(eval-expresion (car valuesItem) env) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                (default-match-exp () (eval-expresion (car valuesItem) env))
                (list-match-exp (id1 id2) (if (list? (eval-expresion item env)) (eval-expresion (car valuesItem) (extend-env (list id1) (list (direct-target (car (eval-expresion item env)))) (extend-env (list id2) (list (direct-target (cdr  (eval-expresion item env)))) env))) (eval-match-exp item (cdr cases-regular-exp) (cdr valuesItem) env)))
                )
              )
      
       
    )
)

(define (extract-id exp)
  (cases expresion exp
    (var-exp (id) id)
    (else (eopl:error 'extract-id "No es un identificador")))
  )

(define (replace-in-list lst index new-value)
  (define (replace-aux lst current-index)
    (if (null? lst)
        '()
        (if (= current-index index)
            (cons new-value (replace-aux (cdr lst) (+ current-index 1)))
            (cons (car lst) (replace-aux (cdr lst) (+ current-index 1))))))
  (if (or (< index 0) (>= index (length lst)))
      (eopl:error "Índice fuera de rango")
      (replace-aux lst 0)))  

(define searchItem
  (lambda (exp listId)
    (let ((pos (rib-find-position exp listId)))
      (if (number? pos)
          pos
          (eopl:error 'searchItem "No binding for ~s" exp)
      ) 
    )
  )
)

(define extractItem
  (lambda (pos listId)
    (list-ref listId pos)
    ))

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids args env))))))

(define eval-struct-decl
  (lambda (structs)
    (cases struct-decl structs
      (struct-exp (id ids) (list id (map (lambda (x) x) ids)))
    )
    )
  )

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expresion rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expresion rand env))))))

(define eval-for-exp 
  (lambda (itdor from until by body env)
    (let ((itdor-val (eval-expresion (var-exp itdor) env)))
      (cond  
      [(<= itdor-val (eval-expresion until env))
         (begin
           (eval-expresion body env)
           (setref! (apply-env-ref env itdor) (+ itdor-val (eval-expresion by env)))
           (eval-for-exp itdor from until by body env))]
      [else #f]))
  )
)

(define eval-switch-exp
  (lambda (item cases caseValues defaultValue env)
    (cond
      [(or (null? cases) (null? caseValues) ) defaultValue] 
      [(eqv? (eval-expresion item env) (car cases)) (car caseValues)]
      [else (eval-switch-exp item (cdr cases) (cdr caseValues) defaultValue env)]
      )
    )
  )


(define apply-while-exp (
  lambda (condicion cuerpo env)
  (cond
      [(eqv? (eval-expresion condicion env) #t)
      (eval-expresion cuerpo env) (apply-while-exp condicion cuerpo env)]
      [else #f]
      )
  )
)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym))))
            )))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define aplicar-primitiva-array
  (lambda (prim exps)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car exps)))
      (index-primArr () (vector-ref (car exps) (cadr exps)))
      (slice-primArr () (vector-copy (car exps) (cadr exps) (+ 1 (caddr exps))))
      (setlist-primArr () (vector-set! (car exps) (cadr exps) (caddr exps)) (car exps))
      )
    )
  )

(define aplicar-primitiva-listas
  (lambda (prim exp)
    (cases primitivaListas prim
      (first-primList () (car exp))
      (rest-primList () (cdr exp))
      (empty-primList () (null? exp)))))

(define (contains-set-exp? exp)
    (cases expresion exp
      (begin-exp (id rhs-exp) (contains-set-exp? id))
      (while-exp (condicion exp) (contains-set-exp? exp))
      (for-exp (itdor from until by body) (contains-set-exp? body))
      (if-exp (condicion then elses) (or (contains-set-exp? then) (contains-set-exp? elses)))
      (match-exp (item regular-exp casesValue) (car (map (lambda (x) (contains-set-exp? x)) casesValue)))
      (set-struct-exp (id exp1 exp2) #t)
      (set-exp (id exps) #t)
      (else #f)))

(define eval-decl-exp
  (lambda (decl env)
    (cases var-decl decl
      (let-exp (ids rands body)
               (if (contains-set-exp? body)
                   (eopl:error "No se puede modificar la ligadura de una variable en una declaración let")
                   (let ((args (eval-var-exp-rands rands env)))
                     (eval-expresion body (extend-env ids args env)))))
      (lvar-exp (ids rands body)
                (let ((args (eval-var-exp-rands rands env)))
                  (eval-expresion body (extend-env ids args env))))
      )))

(define cadena-expression
  (lambda (text ltexts)
    (string-append text (apply string-append (map (lambda (lt) (string-append " " lt)) ltexts)))
    )
  )

(define aplicar-primitiva-cadena
  (lambda (prim exps)
    (cases primitivaCadena prim
      (concat-primCad () (apply string-append exps))
      (length-primCad () (string-length (car exps)))
      (index-primCad () (string (string-ref (car exps) (cadr exps))))
      )
    ))

(define aplicar-primitiva-bool
  (lambda (prim exps)
    (cases primitivaBooleana prim
      (and-prim () (todos-iguales? exps))
      (or-prim () (unTrue? exps))
      (xor-prim () (trueExclusivo? exps 0))
      (not-prim () (map (lambda (x) (not x)) exps))
      ))
  )

(define (trueExclusivo? lst acc)
  (cond
    [(null? lst) (if (= acc 1) #t #f)]
    [(> acc 1) #f]
    [(eqv? (car lst) #t) (trueExclusivo? (cdr lst) (+ acc 1))]
    [else (trueExclusivo? (cdr lst) acc)]
    )
  )

(define (unTrue? lst)
  (cond
    [(null? lst) #f]
    [(eqv? (car lst) #t) #t] 
    [else (unTrue? (cdr lst))])) 

(define (todos-iguales? lst)
  (cond
    [(null? lst) #t]
    [(null? (cdr lst)) #t]
    [else
     (let loop ([primero (car lst)] [resto (cdr lst)])
       (if (null? resto)
           #t 
           (if (eqv? primero (car resto)) (loop primero (cdr resto)) 
               #f)
           )
       )
     ]))

(define eval-bool-exp
  (lambda (bool)
    (cases bool-expresion bool
      (true-exp () #t)
      (false-exp () #f)
      )
    )
  )

(define eval-numero-exp
  (lambda (num)
    (cases numero-exp num
      (decimal-num (num) num)
      (octal-num (num) (convertir-string num))
      (bin-num (num) (convertir-string num))
      (hex-num (num) (convertir-string num))
      (float-num (num) num)
      )
    ))

(define aplicar-primitiva
  (lambda (prim exp1 exp2)
    (cases primitiva prim
      (sum-prim () (operando-numeros + exp1 exp2 #f))
      (minus-prim () (operando-numeros - exp1 exp2 #f))
      (mult-prim () (operando-numeros * exp1 exp2 #f))
      (div-prim () (operando-numeros / exp1 exp2 #f))
      (mayor-prim () (operando-numeros > exp1 exp2 #t))
      (menor-prim () (operando-numeros < exp1 exp2 #t))
      (mayorigual-prim () (operando-numeros >= exp1 exp2 #t))
      (menorigual-prim () (operando-numeros <= exp1 exp2 #t))
      (igual-prim () (operando-numeros = exp1 exp2 #t))
      (diferente-prim () (operando-numeros not (= exp1 exp2 #t)))
      (mod-prim () (operando-numeros modulo exp1 exp2 #t))
      (elevar-prim () (operando-numeros expt exp1 exp2 #t))
      )
    )
  )

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))
    )
  )

(define eval-var-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-var-exp-rand x env))
         rands)))

(define eval-var-exp-rand
  (lambda (rand env)
    (direct-target (eval-expresion rand env))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

(interpretador)
