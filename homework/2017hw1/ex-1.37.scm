#lang racket
(define (cont-frac-iter N D k)
;;MY CODE BEGIN
	(let loop ((i 1))
		(if (> i k) 0
			(/ (N i) (+ (D i) (loop (+ i 1)))))))
;;MY CODE END
(cont-frac-iter (lambda (x) x) 
	(lambda (x) 1.0)
	30)
 
(cont-frac-iter (lambda (x) (* 2 x))
	(lambda (x) (* 1.0 x))
	30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
	(if (eq? k eof)
		(void)
		(begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)
