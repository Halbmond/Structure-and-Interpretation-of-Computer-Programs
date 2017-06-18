#lang racket

(define CASE (read))

(for ([tCASE CASE])
	(define N (read))
	(define a (build-list N (lambda (x) (read))))
	(for* ([i N] [j (range i N)])
		(define ai (list-ref a i))
		(define aj (list-ref a j))
		(if (= 1 (gcd ai aj))
			(printf "(~a ~a) " ai aj) (void)))
	(printf "\n")
)


#|
样例输入
2
5 2 4 6 7 9
2 3 4
样例输出
(2 7) (2 9) (4 7) (4 9) (6 7) (7 9)
(3 4)
|#
