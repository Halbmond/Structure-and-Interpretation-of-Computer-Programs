#lang racket

(require racket/stream)



(define (stream-add s1 s2)
	(stream-map + s1 s2))

(define ones (stream-cons 1 ones))

(define integers
	(stream-cons 1 (stream-add ones integers))
)

;(printf "~a\n" (stream-first integers))

;(printf "~a\n" (stream-first (stream-rest integers)))
(printf "~a\n" (stream-ref integers 0))

;(printf "~a\n" (stream-first integers))
