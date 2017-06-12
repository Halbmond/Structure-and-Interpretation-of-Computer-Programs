#lang racket
(define (square x) (* x x))
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

;---------- about tags:
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

;--------- rectangular complex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

;----------- polar complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))



;----------- rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'numer
       (lambda (x) (numer (contents x)))) ; x is tagged rational
  (put 'rational 'denom
       (lambda (x) (denom (contents x)))) ; x is tagged rational

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (void))
(define (make-rational n d)
  ((get 'make 'rational) n d))




;---------- high level complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                       (+ (angle z1) (angle z2))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                       (- (angle z1) (angle z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make-from-real-imag (- (* a c) (* b d)) (+ (* a d) (* b c)))))

 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (+ (square c) (square d))))
       (make-from-real-imag (/ (+ (* a c) (* b d)) denom)
                            (/ (- (* b c) (* a d)) denom)))))

  ;(a+bi)/(c+di) =(a+bi)*(c-di)/(c+di)*(c-di)=(ac-adi+bci+bd)/(c*c+d*d)=(ac+bd)/(c^2+d^2)+〔(bc-ad)/(c^2+d^2)〕i  
  
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

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

;---------------- real package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  (void))

(define (make-real x)
  ((get 'make 'real) x))


;----------- general functions
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))






(define (install-raise-package) ;install raise functions 
  (define (raise-integer n)
    (make-rational (contents n) 1))
;begin
  (put 'raise 'integer raise-integer)
  (put 'raise 'rational (lambda (x) (make-real (/ ((get 'rational 'numer) x) ((get 'rational 'denom) x)))))
  (put 'raise 'real (lambda (x) (make-complex-from-real-imag (contents x) 0)))
)
(define (conv x t)
  (cond
    ((eq? (type-tag x) t) x)
    ((eq? (type-tag x) 'complex) null)
    (else (conv ((get 'raise (type-tag x)) x) t))
  )
)
(define (apply-generic op . args)
  (if (null? (cdr args))
    (let ((x (car args)))
       ((get op (list (type-tag x))) (contents x))
    )
    (let* ((x (car args)) (y (cadr args)) (x2 (conv x (type-tag y))) (y2 (conv y (type-tag x))))
      (if (null? y2)
        ((get op (list (type-tag y) (type-tag y))) (contents x2) (contents y))
        ((get op (list (type-tag x) (type-tag x))) (contents x) (contents y2))
      )
    )
  )
)
(define (tolst x) (if (pair? x) (cons (car x) (tolst (cdr x))) (list x)))
(define (display-obj x) (let ((l (tolst x)))
  (if (eq? (car l) 'complex)
    (displayln (list* 'complex (list-tail l 2)))
    (displayln l)
  )
))
;end
(install-integer-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)
(install-raise-package)


(define real-1 (make-real 3.0))
(define rational-1 (make-rational 4 3))
(define integer-1 (make-integer 2))
(define complex-1 (make-complex-from-real-imag 30 40))
(displayln "*****1")
(display-obj integer-1)
(display-obj real-1)
(display-obj rational-1)
(display-obj complex-1)

(displayln "*****2")
(display-obj ((get 'raise 'integer) integer-1)) ;interger converted to rational
(display-obj ((get 'raise 'rational) rational-1)) ;rational converted to real
(display-obj ((get 'raise 'real) real-1)) ;real converted to complex

(displayln "*****3")
(display-obj (add real-1 integer-1))
(display-obj (add real-1 rational-1))
(display-obj (add real-1 complex-1))
(display-obj (add complex-1 integer-1))
(display-obj (add integer-1 rational-1))

(displayln "******")

;a tagged complex is like: (cons 'complex (cons 'rectangular (cons 3 4))),displayed in racket as '(complex rectangular 3 . 4) 

(define (myloop)
  (define (make-obj lst)
    (let ((t (car lst)))
      (cond ((eq? t 'integer) (make-integer (cadr lst)))
            ((eq? t 'rational) (make-rational (cadr lst) (caddr lst)))
            ((eq? t 'real) (make-real (cadr lst)))
            ((eq? t 'complex) (make-complex-from-real-imag (cadr lst) (caddr lst))))))

  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (let ((op (car exp))
              (a1 (make-obj (cadr exp)))
              (a2 (make-obj (caddr exp))))
          (display-obj (cond ((eq? op '+) (add a1 a2))
                ((eq? op '-) (sub a1 a2))
                ((eq? op '*) (mul a1 a2))
                ((eq? op '/) (div a1 a2))))
          (myloop)))))
(myloop)