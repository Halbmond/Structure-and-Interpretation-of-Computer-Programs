#lang lazy
;; An infinite list:
(define fibs
  (list* 1 1 (map + fibs (cdr fibs))))
 
;; Print the 1000th Fibonacci number:
(print (list-ref fibs 1000))
