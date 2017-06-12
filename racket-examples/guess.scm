#lang racket/gui
(define f (new frame% [label "Guess"]))
(define n (random 5)) (send f show #t)
(define ((check i) btn evt)
    (message-box "." (if (= i n) "Yes" "No")))
(for ([i (in-range 5)])
    (make-object button% (~a i) f (check i)))