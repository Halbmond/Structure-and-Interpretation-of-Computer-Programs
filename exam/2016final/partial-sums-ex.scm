#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)
;MY CODE BEGIN
 	(define (partial-sums op s)
		(define (partial-sums-and-add op s0 s)
			(cons-stream (op s0 (stream-car s)) (partial-sums-and-add op (op s0 (stream-car s)) (stream-cdr s))))
		(cons-stream (stream-car s) (partial-sums-and-add op (stream-car s) (stream-cdr s))))
 	(define (stream-even-index s)
 		(cons-stream (stream-car s) (stream-odd-index (stream-cdr s))))
 	(define (stream-odd-index s)
		(stream-even-index (stream-cdr s)))
 	(define (stream-union s1 s2)
 		(cons-stream (stream-car s1) (stream-union s2 (stream-cdr s1))))
	(stream-union
		(partial-sums op (stream-even-index s))
		(partial-sums op (stream-odd-index s))))
;MY CODE END
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)
#|
#lang racket
;book ex 3.55
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
	(syntax-rules ()
		[(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
				 
(eval '(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
	(if (stream-null? s) the-empty-stream
			(if (= n 0)
					(stream-car s)
					(stream-ref (stream-cdr s) (- n 1)))))
			env)


(eval '
 (define (partial-sums op s) ;以s为参数,返回的流是 s0,(op s0 s1),(op s0 s1 s2), ....
;MY CODE BEGIN (define (partial-sums op s) ;以s为参数,返回的流是 s0,(op s0 s1),(op s0 s1 s2), ....
	(define (partial-sums-and-add op s0 s)
		(cons-stream (op s0 (stream-car s)) (partial-sums-and-add op (op s0 (stream-car s)) (stream-cdr s))))
	(cons-stream (stream-car s) (partial-sums-and-add op (stream-car s) (stream-cdr s))))
;MY CODE END
env)

(define (myloop)
	(define (eval-codes codes last-val)
		(if (null? codes)
				last-val
				(eval-codes (cdr codes) (eval (car codes) env))))
		
	(let ((codes (read)))
		(if (eq? codes eof)
				(void)
				(begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)
|#