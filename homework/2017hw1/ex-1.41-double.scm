#lang racket
(define (inc x) (+ x 1))
(define (square x ) (* x x))
(define (doubleF f)
;;MY CODE BEGIN
  (lambda (x) (f (f x))))
;;MY CODE END
((doubleF square) 10)
(define X (doubleF (doubleF doubleF)))
((X inc) 5)
(((doubleF (doubleF (doubleF doubleF))) inc) 5) ;Êä³ö261 

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display ((X inc) k)) 
               (newline) (myloop)))))

(myloop)
