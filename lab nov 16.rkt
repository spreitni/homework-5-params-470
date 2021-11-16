;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |lab nov 16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#|
FIrst attempt decided to go with an easier block exp
;(A (B (C (1) (2) (D (3)(4))) (E (F (5)(6))(G(7)(8)))))
(define helper
  (lambda (block-exp)
    (cond
      ((null? block-exp)'())
      ((list? block-exp)(helper (car block-exp)))
      ((symbol? (car block-exp-list)) (list (car block-exp-list) (helper (cdr block-exp-list))))
      (else(helper (cdr block-exp))))))
(define collect-symbols
  (lambda(block-exp-list)
    (cond
      ((null? block-exp-list)(collect-symbols(cdr block-exp-list)))
      ((symbol? (car block-exp-list)) (list (car block-exp-list) (
      (else (display 1)))))
#|(define count 0)
(define count-block-exp
  (lambda (block-exp)
    (cond
      ((null?
      ((number? (car block-exp))((+ count (car block-exp)) (count-block-exp (cdr block-exp))))
      ((list? block-exp) (count-block-exp (cdr block-exp)))
      (else (count-block-exp (cdr block-exp))))))|#
|#

(define collect-symbols
  (lambda (block-exp)
    (cond
      ((null? block-exp)'())
      ((symbol? (car block-exp))(cons (car block-exp) (collect-symbols (cdr block-exp))))
      ((list? (car block-exp)) (collect-symbols  (car block-exp)))
      (else(collect-symbols (cdr block-exp))))))

(define count-block-exp 
  (lambda (block-exp)
    (cond
      ((null? block-exp) '())
      ((number? (car block-exp))(+ (car block-exp) (count-block-exp (cdr block-exp))))
      ((list? (car block-exp)) (count-block-exp  (cdr block-exp)))
      (else(count-block-exp (cdr block-exp))))))



    



(define block-exp '(1 A (2 B (3 C (4 D (5 E(6 F(7 G 8))))))))
;(collect-symbols block-exp)
(count-block-exp block-exp)
;(display (caddr(caddr block-exp)))