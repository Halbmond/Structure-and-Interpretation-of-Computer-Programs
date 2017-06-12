#lang racket
(define (pipeline operand . ops)
;MY CODE BEGIN
	(if (null? ops) operand
		(apply pipeline (cons ((car ops) operand) (cdr ops)))
	)
	;;((foldl compose values ops) operand)
;MY CODE END
)
(define (fib n) ;get the nth item of Fibonacci
	(define (helper a b i)
		(if (> i n)
				b
				(helper b (+ a b) (+ i 1))))
	(if (= n 0)
			0
			(helper 0 1 2)))

(define (accumulate op init seq)
	(if (null? seq)
			init
			(op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
	(if (> a b)
			'()
			(cons a (enumerate-interval (+ a 1) b))))


(define (even-fibs n)
	(pipeline (enumerate-interval 1 n)
						(lambda (lst) (map fib lst))
						(lambda (lst) (filter even? lst))
						(lambda (lst) (accumulate cons '() lst))))
(display (even-fibs 9)) (newline)
(display (even-fibs 19)) (newline)

(display "******") (newline)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (f1 lst)
	(if (not (pair? (car lst)))
			(pipeline lst
								(lambda (x) (map square x))
								(lambda (x) (map inc x))
								(lambda (x) (map inc x))
								(lambda (x) (map inc x)))
			(pipeline lst
								(lambda (x) (map car x))
								(lambda (x) (map inc x))
								(lambda (x) (map square x))
								(lambda (x) (map inc x))
								(lambda (x) (map inc x)))))

(define (myloop)
	(let ((a (read)))
		(if (eq? a eof)
				(void)
				(begin (display (f1 a)) (newline)
							 (myloop)))))

(myloop)


#|
样例输入
(1 2 3 4)
((1) (2 4) (3 5) (4 6) (5 7))
样例输出
(2 8 34)
(2 8 34 144 610 2584)
******
(4 7 12 19)
(6 11 18 27 38)
|#
