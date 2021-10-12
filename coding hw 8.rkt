;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |coding hw 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))
(define extend-env-let
  (lambda (var val env)
    (cond
      ((null? var) env)
      (else (cons(list var val) env)))))

(define do-let-exp
 (lambda (var1 val1 var2 val2 math env)
    (do-mathy-stuff-toaster (car math)
                            (if (eq? var1 (cadr math))
                                (resolve var1 (extend-env-let var1 val1 env))
                                (resolve var2 (extend-env-let var2 val2 env)))
                            (if (eq? var2 (caddr math))
                                (resolve var2 (extend-env-let var2 val2 env))
                                (resolve var1 (extend-env-let var1 val1 env))))))
    
(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

; ***** PARSERS *****
(define boolean-expression-parser
  (lambda (boolean-expression)
    (cond
      ((eq? (car boolean-expression) '<)
       (list 'less-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '<=)
       (list 'less-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>)
       (list 'greater-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>=)
       (list 'greater-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '==)
       (list 'equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '!=)
       (list 'not-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      (else "Not a valid boolean expression"))))

(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))
(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'let)
       (list 'let-exp (cdr no-code)))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
(define run-parsed-boolean-code
  (lambda (parsed-boolean-code env)
    (cond
      ((eq? (car parsed-boolean-code) 'less-then)
       (<
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'less-then-or-equal)
       (<=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then)
       (>
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then-or-equal)
       (>=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'equal)
       (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'not-equal)
       (not (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env))))
      (else "Not a legal boolean expression!"))))
       
        
    
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

           
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'let-exp)
       (do-let-exp
        (caar(caadr parsed-no-code))
        (cadar(caadr parsed-no-code))
        (caadr(caadr parsed-no-code))
        (car(cdadr(caadr parsed-no-code)))
        (car(cdadr parsed-no-code)) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env) 
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(ask (!= 6 5) (let ((f 23)(g 8)) (- g f)) otherwise c)) ;change code to work with ((f 2)(g 3))call extend env on the caar caadr cdadr cdaddr
(define parsed-no-code (no-parser sample-no-code))
;parsed-no-code
(run-parsed-code parsed-no-code env)

