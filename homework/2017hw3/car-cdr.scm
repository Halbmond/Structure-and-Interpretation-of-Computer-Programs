#lang racket
(define (cons x y)
  (lambda (m) (m x y)))

;MY CODE BEGIN
(define (fi x y) x)
(define (se x y) y)
(define (car f) (f fi))
(define (cdr f) (f se))
;MY CODE END

(displayln (car (cons 1 2)))
(displayln (cdr  (cons 1 2)))
(displayln (car (cons 100 (list 2 3))))
(displayln (cdr (cons 13 (list 1 66 7 3))))
(define z (cons 100 (cons 200 (cons 300 (cons 400 '())))))
(displayln (car (cdr z)))
(displayln (car (cdr (cdr z))))
(displayln (cdr (cons 1 '())))


#|
输入
无
输出
1
2
100
(1 66 7 3)
200
300
()
|#
