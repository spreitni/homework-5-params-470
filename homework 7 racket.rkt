;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |homework 7 racket|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))
(define no-code-ask-parser
  (lambda (no-code-ask)
    (list 'ask-exp
             (list 'bool-exp (caadr no-code-ask)(no-parser (cadr(cadr no-code-ask)))(no-parser (caddr(cadr no-code-ask))))
             (no-parser (caddr no-code-ask))
             (no-parser (car (reverse no-code-ask))))))

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (no-code-ask-parser no-code))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

(define run-parsed-ask-code
  (lambda (op parsed-no-code-ask env) ;bool exp
    (cond
      ((eq? op '>) (if(>(cadar(cddar parsed-no-code-ask))(cadr(cadr(cddar parsed-no-code-ask))))
           (run-parsed-code (cadr parsed-no-code-ask) env)
           (run-parsed-code (caddr parsed-no-code-ask) env)))
      ((eq? op '<) (if(<(cadar(cddar parsed-no-code-ask))(cadr(cadr(cddar parsed-no-code-ask))))
           (run-parsed-code (cadr parsed-no-code-ask) env)
           (run-parsed-code (caddr parsed-no-code-ask) env)))
      ((eq? op '>=) (if(>=(cadar(cddar parsed-no-code-ask))(cadr(cadr(cddar parsed-no-code-ask))))
           (run-parsed-code (cadr parsed-no-code-ask) env)
           (run-parsed-code (caddr parsed-no-code-ask) env)))
      ((eq? op '<=) (if(<=(cadar(cddar parsed-no-code-ask))(cadr(cadr(cddar parsed-no-code-ask))))
           (run-parsed-code (cadr parsed-no-code-ask) env)
           (run-parsed-code (caddr parsed-no-code-ask) env)))
      ((eq? op '=) (if(=(cadar(cddar parsed-no-code-ask))(cadr(cadr(cddar parsed-no-code-ask))))
           (run-parsed-code (cadr parsed-no-code-ask) env)
           (run-parsed-code (caddr parsed-no-code-ask) env)))
      (else (display 2)))))
                   
;(display (cadar(cddar parsed-no-code-ask)))))
;(ask (< 1 3) (do-mathy-stuff + x y) otherwise 5)
           
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
      ((eq? (car parsed-no-code) 'ask-exp) ;call ask code here passing the op and rest of   bool-exp and env
       (run-parsed-ask-code (car(cdadr parsed-no-code))(cdr parsed-no-code) env))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x y) (ask (< 4 3) (do-mathy-stuff + x y) otherwise 5)) (do-mathy-stuff * a b) 15))
(define parsed-no-code (no-parser sample-no-code))
parsed-no-code
(run-parsed-code parsed-no-code env)