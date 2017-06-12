#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)
;MY CODE BEGIN
	(cond [(= n 0) identity]
		[else (lambda (x) ((repeated f (sub1 n)) (f x)))]
	)
)
;MY CODE END
((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)

#|
输入
每组数据一行，每行是一个整数n
输出
对每个整数n，输出以2作为参数调用题目中的square函数n次复用后的函数 的返回值
样例输入
1
2
3
4
5
样例输出
625
10
96
********
4
16
256
65536
4294967296
|#
