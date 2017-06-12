#lang racket

;; Print the Greek alphabet
(for ([i 25])
  (displayln
   (integer->char
    (+ i (char->integer #\α)))))
