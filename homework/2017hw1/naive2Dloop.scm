#lang racket
(let loop ([a 0] [b 0])
	(unless (eq? b eof)
		(for ([i a])
			(for ([j b]) (printf "~a," (+ 1 j)))
			(newline))
		(loop (read) (read))))