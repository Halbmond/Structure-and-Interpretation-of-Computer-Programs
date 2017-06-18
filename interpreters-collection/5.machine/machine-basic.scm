;basic machine interpreter
#lang racket
;machine interpreter
(require racket/mpair)

(define (mtagged-list? exp tag) 
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))

(define (tagged-list? exp tag) 
  (if (pair? exp) 
      (eq? (car exp) tag)
      false))


(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name) 
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      (void))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))


(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine) 
  
  (let ((pc (make-register 'pc)) 
        (flag (make-register 'flag)) 
        (stack (make-stack))
        (the-instruction-sequence '())) 
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))) 
      (define (allocate-register name) 
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name) 

        (let ((val (assoc name register-table)))
          (if val
              (cadr val)  

              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))  

          (if (null? insts)
              (void)
              (begin
                ((instruction-execution-proc (mcar insts))) 

                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence) 

               (execute)) 

              ((eq? message 'install-instruction-sequence) 

               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) 

               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack) 

              ((eq? message 'operations) the-ops) 
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  (void))

(define (get-register machine reg-name)  

  ((machine 'get-register) reg-name))





(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine) 

      insts)))


(define (extract-labels text receive) 

  (if (null? text)
      (receive '() '()) 

      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
                 (if (assoc next-inst labels)
                     (exit)
                     (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (mcons (make-instruction next-inst)  

                              insts)
                        labels)))))))


(define (update-insts! insts labels machine) 

  

  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))) 

    (mfor-each 

     (lambda (inst)
       (set-instruction-execution-proc! 
        inst  

        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts))) 





(define (make-instruction text) 

  (mcons text '())) 
(define (instruction-text inst)
  (mcar inst))
(define (instruction-execution-proc inst)
  (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc)) 





(define (make-label-entry label-name insts)
  (cons label-name insts)) 
 

 


(define (lookup-label labels label-name)   

  (let ((val (assoc label-name labels)))
    (if val
        (cdr val) 

        (error "Undefined label -- ASSEMBLE" label-name))))






(define (make-execution-procedure inst labels machine pc flag stack ops) 
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))


(define (make-assign inst machine labels operations pc)
  (let ((target 
         (get-register machine (assign-reg-name inst)))  
        (value-exp (assign-value-exp inst))) 
    (let ((value-proc
           (if (operation-exp? value-exp)  
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))  
      (lambda ()                
        (set-contents! target (value-proc)) 
        (advance-pc pc))))) 



(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))


(define (advance-pc pc) 

  (set-contents! pc (mcdr (get-contents pc))))   



(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst))) 

    (if (operation-exp? condition) 

        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))



(define (make-branch inst machine labels flag pc) 
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc) 

  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))


(define (make-save inst machine stack pc) 

  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))


(define (make-primitive-exp exp machine labels) 
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels  

                              (label-exp-label exp))))
           (lambda () insts))) 

        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))  

        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))  
        (aprocs
         (map (lambda (e)  

                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp) 
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (make-stack-new)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      (void))
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define gcd-machine-text
  '(make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))


(define fib-machine-text
  '(make-machine
   '(a b t n continue val)
   (list (list 'rem remainder) (list '= =) (list '< <) (list '+ +) (list '- -))
   '(fib-start
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   

   (save continue)
fib-loop
   (assign continue (label afterfib-n-1))
   (save n)                           

   (assign n (op -) (reg n) (const 1))

   (goto (label fib-loop))            

 afterfib-n-1                         

   (restore n)
   (restore continue)
   

   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         

   (goto (label fib-loop))
 afterfib-n-2                         

   (assign n (reg val))               

   (restore val)                      

   (restore continue)
   (assign val                        

           (op +) (reg val) (reg n)) 
   (goto (reg continue))              

 immediate-answer
   (assign val (reg n))               

   (goto (reg continue))
 fib-done)))

(define add-machine-text
'(make-machine
   '(c d val)
   (list (list '+ +))
   '(
       assign val (op +) (reg c) (reg d) 
     )))


(define (get-op-table lst)
  (define (change op) 

    (let ((o (cadr op)))
      (cond ((eq? o '=) (cons (cadar op) (list =)))
            ((eq? o '+) (cons (cadar op) (list +)))
            ((eq? o '-) (cons (cadar op) (list -)))
            ((eq? o '*) (cons (cadar op) (list *)))
            ((eq? o '/) (cons (cadar op) (list /)))
            ((eq? o '<) (cons (cadar op) (list <)))            
            ((eq? o '>) (cons (cadar op) (list >)))            
            ((eq? o 'eq?) (cons (cadar op) (list eq?)))            
            ((eq? o 'remainder) (cons (cadar op) (list remainder))))))
  (if (null? lst)
      '()
      (cons (change (cdr (car lst))) (get-op-table (cdr lst)))))

(define (make-machine-from-text machine-text)
  (define reg-table (cadr (cadr machine-text)))
  (define op-table (get-op-table (cdr (caddr machine-text))))
  (define controller (cadr (cadddr machine-text)))
  (make-machine reg-table op-table controller))



(define (run-machine machine input output) 

  (define (init-machine machine input) 
    (if (null? input)
        (void)
        (begin (set-register-contents! machine (caar input) (cadar input))
               (init-machine machine (cdr input)))))
  (define (output-machine machine output)
    (if (null? output)
        (void)
        (begin (display (get-register-contents machine (car output))) (display " ") 
               (output-machine machine (cdr output)))))
  (init-machine machine input)
  (start machine)
  (output-machine machine output)
  (newline))
  


(define (process-loop)
  (let ((machine '*unassigned*))
    (define (inner-loop)
      (let ((m-txt (read)))
            (if (eq? m-txt eof)
                (void)
                (if (eq? (car m-txt) 'make-machine)
                    (begin (set! machine (make-machine-from-text m-txt))
                           (display "a new machine") (newline)
                           (run-machine machine (read) (read))
                           (inner-loop))
                    (begin (run-machine machine m-txt (read)) 
                           (inner-loop))))))
    (inner-loop)))


(process-loop)


