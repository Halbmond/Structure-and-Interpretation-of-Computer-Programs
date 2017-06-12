#lang racket
(let ([saw (make-hash)])
  (for ([line (in-lines)])
    (unless (hash-ref saw line #f)
      (displayln line))
    (hash-set! saw line #t)))
