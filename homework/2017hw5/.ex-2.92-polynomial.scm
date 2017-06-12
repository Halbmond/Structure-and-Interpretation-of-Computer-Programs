#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;begin
;(polynomial a (2 (integer . 3))  (1 (integer . 2)) (0 (integer . 1)))
(define (install-polynomial-package)
  (define (tag x) (attach-tag 'polynomial x))
  (define (get-v-str x) (symbol->string (car x)))
  (define (raise-v x y) (cons x (list (car x) (make-term 0 (tag y)))))
  (define (same-var x y)
    (cond 
      ((eq? (car x) (car y)) (cons x y))
      ((string<? (get-v-str x) (get-v-str y)) (raise-v x y))
      (else (raise-v y x))
    )
  )
  (define (add-t x y) (cond
    ((null? x) y)
    ((null? y) x)
    ((= (caar x) (caar y)) (cons (make-term (caar x) (add (cadar x) (cadar y))) (add-t (cdr x) (cdr y))))
    ((> (caar x) (caar y)) (cons (car x) (add-t (cdr x) y)))
    (else (cons (car y) (add-t x (cdr y))))
  ))
  (define (mul-t-s x t) (map (lambda (xt) (make-term (+ (car t) (car xt)) (mul (cadr t) (cadr xt)))) x))
  (define (mul-t x y) (foldl (lambda (xt res) (add-t (mul-t-s y xt) res)) '() x))
  (define (addp x y) (let ((res (same-var x y))) (cons (caar res) (add-t (cdar res) (cddr res)))))
  (define (mulp x y) (let ((res (same-var x y))) (cons (caar res) (mul-t (cdar res) (cddr res)))))
  (put 'make 'polynomial (lambda (v t) (tag (cons v t))))
  (put 'make 'polynomial-term (lambda (o c) (list o c)))
  (put 'add '(polynomial polynomial) (lambda (x y) (tag (addp x y))))
  (put 'mul '(polynomial polynomial) (lambda (x y) (tag (mulp x y))))
)
(define (conv x y)
  (cond
    ((eq? (type-tag x) (type-tag y)) x)
    ((eq? (type-tag x) 'integer) (make-poly (cadr y) (list (make-term 0 x))))
    (else null)
  )
)
(define (apply-generic op . args)
  (if (null? (cdr args))
    (let ((x (car args)))
       ((get op (list (type-tag x))) (contents x))
    )
    (let* ((x (car args)) (y (cadr args)) (x2 (conv x y)) (y2 (conv y x)))
      (if (null? y2)
        ((get op (list (type-tag y) (type-tag y))) (contents x2) (contents y))
        ((get op (list (type-tag x) (type-tag x))) (contents x) (contents y2))
      )
    )
  )
)
(define (build-poly a)
  (if (pair? a)
    (make-poly (car a) (map (lambda (x) (make-term (car x) (build-poly (cadr x)))) (cdr a)))
    (make-integer a)
  )
)
(define (d-poly a)
  (if (eq? (type-tag a) 'integer)
    (contents a)
    (cons (cadr a) (foldr (lambda (x y) (cons (list (car x) (d-poly (cadr x))) y)) '() (cddr a)))
  )
)
(define (display-poly a) (displayln (d-poly a)))
;end
(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)