#lang racket
(for ([CASE (in-naturals)]) (displayln (remove-duplicates (sort (set-union (read) (read)) <))))
