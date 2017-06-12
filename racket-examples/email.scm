#lang racket ; Sending email from racket
(require net/sendmail)
(sleep (* (- (* 60 4) 15) 60)) ; 4h - 15m
(send-mail-message
	(getenv "EMAIL") "Parking meter alert!"
	(list (getenv "EMAIL")) null null
		'("Time to go out and move your car."))
