
(define closure-demo
	(let ([y 5])
		(lambda (x)
			(set! y (+ y x)) ; (set! a b) 等价于赋值语句 a = b;
			y
		)
	)
)

#|
(closure-demo 6)
=> 11
(closure-demo 10)
=> 21
|#


;用闭包实现面向对象




(define (closure-demo)
	(let ([y 5] [k (display "kk")])
		(lambda (x)
			(set! y (+ y x))
			y
		)
	)
)

((closure-demo) 6) ;=>kk11
((closure-demo) 10) ;=>kk15
(define x (closure-demo)) ;=>kk
(x 6) ;=>11
(x 10);=>21

