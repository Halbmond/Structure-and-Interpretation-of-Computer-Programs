#lang racket
(void (let* ([R (range 1 9)]
    [A (filter-not
        (lambda (a) (or (check-duplicates (map + a R))
                        (check-duplicates (map - a R))))
        (permutations R))])
    (map (lambda (T)
            (map display (reverse (list-ref A (- 92 (read)))))
            (newline))
        (range (read)))))

#|
(void (let ([R (range 1 9)])
    (map (lambda (T)
        (map display (list-ref (filter (lambda (a) (define (L op) (length (remove-duplicates (map op a R)))) (= (+ (L +) (L -)) 16)) (map reverse (permutations R))) (- 92 (read))))
        (newline))
        (range (read)))))
|#
#|
(void (let ([R (range 1 9)])
    (define (check a)
        (define (L op) (length (remove-duplicates (map op a R))))
        (= (+ (L +) (L -)) 16))
    #|
    (define (check a)
        (= 16 (length (append (remove-duplicates (map + a R)) (remove-duplicates (map - a R))))))
    |#
    (map (lambda (T)
        (map display
            (list-ref (filter check (map reverse (permutations R))) (- 92 (read))))
        (newline))
        (range (read)))))
|#
#|
(void
    (let* ([R (range 1 9)]
        [A  (filter
                (lambda(a)
                    (define (L op) (length (remove-duplicates (map op a R))))
                    (= ( + (L +) (L -)) 16))
                (map reverse (permutations R)))])
        (map
            (lambda(T)
                (map display (list-ref A (- 92 (read))))
                (newline))
            (range (read)))))
|#
