#lang racket

(define R (read))
(define C (read))

(define A (build-vector R (lambda (i) (build-vector C (lambda (j) (read))))))
(define (vref A i j) (vector-ref (vector-ref A i) j))
(define (vset A i j v) (vector-set! (vector-ref A i) j v))


(define F (build-vector R (lambda (i) (build-vector C (lambda (j) 1)))))


(define (dfs x y)
	(when (eq? (vref F x y) 1)
		(define v (vref A x y))
		(for-each (lambda (dx)
				(for-each (lambda (dy)
					(when (= 1 (+ (abs dx) (abs dy)))
						(define x1 (+ x dx))
						(define y1 (+ y dy))
						(when (and (<= 0 x1) (< x1 R) (<= 0 y1) (< y1 C))
							(when (> v (vref A x1 y1))
								(vset F x y (max (vref F x y) (+ 1 (dfs x1 y1))))
							)
						)
					))
					'(-1 0 1)
				)
			)
			'(-1 0 1)
		)
	)
	(vref F x y)
)

;(displayln (dfs 1 1))
(displayln (apply max (map
	(lambda (i)
		(apply max (map (lambda (j) (dfs i j))
			(range C)))
	)
	(range R))))

;(displayln A)
;(define F (build-vector (lambda (i) (build-vector (lambda (j) (read)) (range C))) (range R)))
