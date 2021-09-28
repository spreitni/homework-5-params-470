;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |homework params|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))


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
       #|(if(> (length no-code) 1) 
          (no-parser (cdr no-code))
      ((symbol? no-code)
       (if(null?  no-code)
                 (list 'var-exp no-code)
                 (no-parser  no-code)))|#
(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code)(list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append(list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      ((eq? (car no-code) 'call)(list 'call-exp
                  (no-parser (cadr no-code))
                  (no-parser (caddr no-code))  ;works but is hard coded
                  (no-parser (cadddr no-code))
                  (no-parser (cadr(cdddr no-code)))))
      (else ('())))))


      #|(else (list 'call-exp
                  (no-parser (cadr no-code))
                  (no-parser (caddr no-code)))))))|#
    
(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x a) x )a b c ))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (cons (list (cadr (cadr (cadr parsed-no-code)))
                    (run-parsed-code (caddr parsed-no-code) env))
              env))))))
(no-parser sample-no-code)
(run-parsed-code (no-parser sample-no-code) env)