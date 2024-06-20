#lang eopl
(require racket/vector)

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

    ; ; ;;Instanciación y uso de estructuras
    ; (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    ; (expresion ("get" expresion "." identificador) get-struct-exp)
    ; (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ; ; ;;Reconocimiento de patrones
    ; (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)

    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ; ;;primitivas numéricas
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

    ; ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ; ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ; ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ; ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ; ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

    ; ;;Estructuras de datos
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
      (a-programa (struct-decl  exp) (eval-expresion exp ambiente-inicial)
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
   (env environment?)))

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
      (set-exp (id rhs-exp) (begin (setref! (apply-env-ref env id) (eval-expresion rhs-exp env)) ))
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
      )
    ))


(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids args env))))))


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

    (
      (display (apply-env-ref env itdor ))
    (display (setref! (apply-env-ref env itdor) 2))
            (display (apply-env-ref env itdor))
      
      ) 
     
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
                                 (apply-env-ref env sym)))))))

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
  (cond
    [(or (set-exp? exp) (begin-exp? exp)) #t]
    [else #f]))

(define eval-decl-exp
  (lambda (decl env)
    (cases var-decl decl
      (let-exp (ids rands body)
               (if (contains-set-exp? body)
                   (eopl:error "No se puede modificar la ligadura en una declaración let")
                   (let ((args (eval-var-exp-rands rands env)))
                     (eval-expresion body (extend-env ids args env)))))
      (lvar-exp (ids rands body)
                (let ((args (eval-var-exp-rands rands env)))
                  (eval-expresion body (extend-env ids args env))))
      )))

(define begin-exp?
  (lambda (exp)
    (cases expresion exp
      (begin-exp (exp exps) #t)
      (else #f)
      )
    )
)
(define set-exp?
  (lambda (exp)
    (cases expresion exp
      (set-exp (ids idsValues) #t)
      (else #f)
      )
    )
)
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
    [(null? lst) #f] ; Si la lista está vacía, retorna falso
    [(eqv? (car lst) #t) #t] ; Si el primer elemento es verdadero, retorna verdadero
    [else (unTrue? (cdr lst))])) ; De lo contrario, sigue buscando en el resto de la lista

(define (todos-iguales? lst)
  (cond
    [(null? lst) #t] ; Lista vacía, consideramos todos iguales
    [(null? (cdr lst)) #t] ; Solo un elemento, todos iguales
    [else
     (let loop ([primero (car lst)] [resto (cdr lst)])
       (if (null? resto)
           #t ; Llegamos al final sin encontrar diferencias
           (if (eqv? primero (car resto)) (loop primero (cdr resto)) ; Siguiente elemento
               #f)
           )
       )
     ])) ; Encontramos elementos diferentes

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
      (octal-num (num) num)
      (bin-num (num) num)
      (hex-num (num) num)
      (float-num (num) num)
      )
    ))

(define aplicar-primitiva
  (lambda (prim exp1 exp2)
    (cases primitiva prim
      (sum-prim () (+ exp1 exp2))
      (minus-prim () (- exp1 exp2))
      (mult-prim () (* exp1 exp2))
      (div-prim () (/ exp1 exp2))
      (mayor-prim () (> exp1 exp2))
      (menor-prim () (< exp1 exp2))
      (mayorigual-prim () (>= exp1 exp2))
      (menorigual-prim () (<= exp1 exp2))
      (igual-prim () (= exp1 exp2))
      (diferente-prim () (not (= exp1 exp2)))
      (mod-prim () (modulo exp1 exp2))
      (elevar-prim () (expt exp1 exp2))

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
