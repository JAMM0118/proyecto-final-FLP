#lang racket

(provide (all-defined-out))

(define to-string (lambda (x) (cond
  [(string? x) x]
  [(symbol? x) (symbol->string x)]
  [(number? x) (number->string x)]
  )))

(define (string-contains-letters? str)
  (regexp-match? #rx"[A-Za-z]" str))

(define (convertir-string str)
  (if (string-contains-letters? str)
      (string->symbol str)
      (string->number str)))

(define (convertir-decimal num base)
  (cond
    [(eq? base 'd) num]
    [(eq? base 'b) (string->number (string-replace (to-string num) "b" "") 2)]
    [(eq? base 'o) (string->number (string-replace (to-string num) "0x" "") 8)]
    [(eq? base 'h) (string->number (string-replace (to-string num) "hx" "") 16)]
    )
)

(define (crear-representacion num base)
  (convertir-string  (base-prefija-negativa base (cond
    [(eq? base 'd) (number->string num)]
    [(eq? base 'b) (number->string num 2)]
    [(eq? base 'o) (number->string num 8)]
    [(eq? base 'h) (string-upcase (number->string num 16))]
    )))
)

(define base-prefija (lambda (base)
  (cond
    [(eq? base 'd) ""]
    [(eq? base 'b) "b"]
    [(eq? base 'o) "0x"]
    [(eq? base 'h) "hx"]
    )
  )
)

(define base-prefija-negativa (lambda (base num)
  (if (string-contains? num "-")
    (string-append "-" (base-prefija base) (string-replace num "-" ""))
    (string-append (base-prefija base) num)
  ))  
)


(define extraer-base (lambda (str)
  (cond
    [(string-contains? str "b") 'b]
    [(string-contains? str "0x") 'o]
    [(string-contains? str "hx") 'h]
    [else 'd]
    ))
)

(define operando-numeros 
  (lambda (op num1 num2 boolean?)
    (let ([base (extraer-base (to-string num1))]
          [num1 (convertir-decimal num1 (extraer-base (to-string num1)))]
          [num2 (convertir-decimal num2 (extraer-base (to-string num2)))]
          )
      (if boolean?
          (op num1 num2)
          (crear-representacion (op num1 num2) base)
       )
    )
  )
)
