#lang racket

(require scheme/mpair)
(define car mcar)
(define cdr mcdr)
(define cadr (lambda (lst) (mcar (mcdr lst))))
(define list mlist)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)

;;;;;;;;;;;all queue thing
(define (front-ptr que) (car que))
(define (rear-ptr que) (cdr que))

(define (set-front-ptr! que item) (set-car! que item))
(define (set-rear-ptr! que item) (set-cdr! que item))
(define (empty-que? que) (null? (front-ptr que)))

(define (make-que) (cons '() '()))

(define (front-que que)
  (if (empty-que? que) 
      (error "emtpy que" que)
      (car (front-ptr que))))

(define (rear-que que)
  (if (empty-que? que) 
      (error "emtpy que" que)
      (car (rear-ptr que))))

(define (insert-que! que item)
  (let ((new-pair (cons item '())))
    (cond ((empty-que? que)
           (set-front-ptr! que new-pair)
           (set-rear-ptr! que new-pair)
           que)
          (else
           (set-cdr! (rear-ptr que) new-pair)
           (set-rear-ptr! que new-pair)
           que))))
                          

(define (delete-que! que)
  (cond ((empty-que? que) (error "delete empty que" que))
         (else (set-front-ptr! que (cdr (front-ptr que)))
               que)))

(define (print-que que)
  (define (iter ptr)
    (if (null? ptr)
        (display "")
        (begin (display (car ptr)) (iter (cdr ptr)))))
  (iter (front-ptr que)))

;;end of queue thing



(define (call-each procedures) 
  (if (null? procedures)
      (void)
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '())) 
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
               (begin (set! signal-value new-value)
                      (call-each action-procedures)) 
               (void))) ;ori: done-of-set-my-signal
    (define (accept-action-procedure! proc) 
      (set! action-procedures (cons proc action-procedures))
      (proc)) 
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))
      
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (make-time-segment time queue) 
  (cons time queue))

(define (segment-time s) (car s)) 
(define (segment-que s) (cdr s))



(define (make-agenda) (list 0))  
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda)) 

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda) 
  (define (belongs-before? segments)  
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action) 
    (let ((q (make-que)))
      (insert-que! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)  ;add action to segments according to time
  
    (if (= (segment-time (car segments)) time)
        (insert-que! (segment-que (car segments))
                     action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))  
              (add-to-segments! rest)))))
  
  (let ((segments-a (segments agenda)))
    (if (belongs-before? segments-a)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments-a))
        (add-to-segments! segments-a))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-que (first-segment agenda))))
    (delete-que! q)
    (if (empty-que? q)
        (set-segments! agenda (rest-segments agenda))
        (void)))) ;ori: 'ok-remove-first-agenda-item!

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emptyP")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;set the time in agenda to the time of first item when accessing the first item
        (front-que (segment-que first-seg)))))


;the-agenda is the global variable
(define the-agenda (make-agenda))

(define (propagate)  
  (if (empty-agenda? the-agenda)
      (void) ;ori : 'done-agenda-is-empty
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;every thing is happened in the-agentda ,so after-delay is handling the-agenda -- just add action to the-agenda
;then propagate excutes all actions in the-agenda
(define (after-delay delay action) 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))





(define inverter-delay 2) 
(define and-gate-delay 3) 
(define or-gate-delay 5) 




(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (void))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" a1 " " a2))))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "invalid signal" a1 " " a2))))



(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  (void)) ;ori: 'ok-and-gate


(define (or-gate a1 a2 output)  
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  (void)) ;ori: 'ok-orgate

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (void))) ;ori 'ok-half-adder

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    (void))) ;ori 'ok-full-adder


(define (ripple-carry-adder wire-lstA wire-lstB wire-lstS wireCN)
	(define (add-1 AA BB SS C)
		(let ((S (make-wire))
					(Cout (make-wire)))
			(if (null? (cdr AA)) 
					(full-adder (car AA) (car BB) C (car SS) wireCN)
					(begin
						(full-adder (car AA) (car BB) C (car SS) Cout)
						(add-1 (cdr AA) (cdr BB) (cdr SS) Cout)
					)
			)
		)
	)
	(add-1 wire-lstA wire-lstB wire-lstS (make-wire))
)
(define (make-wire-list-N n) ;make a list of n wires
  (if (= n 0)
      '()
      (cons (make-wire) (make-wire-list-N (- n 1)))))

(define (set-values ws vs) ;set the value of wires in ws according to list vs
  (if (null? ws)
      (void)
      (begin (set-signal! (car ws) (car vs)) (set-values (cdr ws) (cdr vs)))))

(define (get-values ws) ;return the list of values of wires in list ws
  (if (null? ws)
      '()
      (cons (get-signal (car ws)) (get-values (cdr ws)))))

(define n (read))
(define A (make-wire-list-N n))
(define B (make-wire-list-N n))
(define S (make-wire-list-N n))
(define C (make-wire))
(ripple-carry-adder A B S C)

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (set-values A (list->mlist a))
               (set-values B (list->mlist b))
               (propagate)
               (displayln (get-values S))
               (displayln (get-signal C))
               (myloop)))))

(myloop)