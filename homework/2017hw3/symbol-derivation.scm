#lang racket
;MY CODE BEGIN
(define (variable? x) (symbol? x))
(define (sum? x) (member '+ x))
(define (product? x) (member '* x))
(define (same-variable? x y) (eq? x y))
(define (-list x) (if (null? (cdr x)) (car x) x))
(define (+list x) (if (list? x) x (list x)))
(define (+plist x) (if (sum? x) (list x) x))
(define (addend x) (-list (takef x (lambda (x) (not (eq? x '+))))))
(define (augend x) (-list (cdr (member '+ x))))
(define (multiplier x) (-list (takef x (lambda (x) (not (eq? x '*))))))
(define (multiplicand x) (-list (cdr (member '* x))))
(define (make-sum x y)
  (cond
    ((eq? x 0) y) ((eq? y 0) x)
    ((and (number? x) (number? y)) (+ x y))
    (else (append (+list x) '(+) (+list y)))))
(define (make-product x y)
  (cond
    ((eq? x 0) 0) ((eq? y 0) 0) ;;ATTENTION (= x 0) will cause  Runtime Error
    ((eq? x 1) y) ((eq? y 1) x)
    ((and (number? x) (number? y)) (* x y))
    (else (append (+plist (+list x)) '(*) (+plist (+list y))))))
;MY CODE END
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))
(myloop)