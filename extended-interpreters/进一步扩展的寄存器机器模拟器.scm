
#lang racket

(require racket/mpair)

(define (mtagged-list? exp tag) 
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))

(define (tagged-list? exp tag) 
  (if (pair? exp) 
      (eq? (car exp) tag)
      false))


(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
;    (for-each (lambda (register-name)
;                ((machine 'allocate-register) register-name))
;              register-names)
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
        (error-flag false)
        (the-instruction-sequence '())) 
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))) 
      (define (allocate-register name) 
        (if (assoc name register-table)
            (void) ;changed for ex2
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
              ((eq? message 'registers) register-table)
              ((eq? message 'set-error-flag) (lambda (flag) (set! error-flag flag) )) 
              ((eq? message 'get-error-flag) error-flag)
              
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

(define (set-error-flag machine flag)
  ((machine 'set-error-flag) flag))

(define (get-error-flag machine)
  (machine 'get-error-flag))

(define (assemble controller-text machine)
  (let ((ret-val (extract-labels controller-text
                                 (lambda (insts labels)
                                   (update-insts! insts labels machine)
                                   insts))))
    (if (eq? ret-val 'error-label)
        (begin (set-error-flag machine ret-val) '())
        ret-val)))
       


(define (extract-labels text receive) 
  (if (null? text)
      (receive '() '()) 
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
                 (if (assoc next-inst labels)
                     'error-label
                     (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (mcons (make-instruction next-inst)  ;changed ,ori: cons
                              insts)
                        labels)))))))


(define (update-insts! insts labels machine) 
  
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (mfor-each ;changed, ori for-each
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




  

(define (make-execution-procedure inst labels machine 
                                  pc flag stack ops)
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
        ((eq? (car inst) 'swap)
         (make-swap inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (add-reg-list machine reg-lst) 
  (if (null? reg-lst)
      (void)
      (if (eq? (caar reg-lst) 'reg)
          (begin ((machine 'allocate-register) (cadr (car reg-lst))) (add-reg-list machine (cdr reg-lst)))
          (add-reg-list machine (cdr reg-lst)))))
  
  
(define (make-assign inst machine labels operations pc);为 assign指令inst生成一个可执行过程, assign指令形如： (assign n (reg b) ) n是regname

  ;(assign <register-name> (reg <register-name>))
  ;(assign <register-name> (const <constant-value>))
  ;(assign <register-name> (op <operation-name>) 
  ;                          <input1> ... <inputn>)
  ;(assign <register-name> (label <label-name>))
  ;上面<inputi>是  (reg <register-name>) 或 (const <constant-value>).
  (define (add-register) 
    ((machine 'allocate-register) (assign-reg-name inst))
    (if (eq? (car (caddr inst)) 'reg)
        ((machine 'allocate-register) (cadr (caddr inst)))
        (if (eq? (car (caddr inst)) 'op)
            (add-reg-list machine (cdddr inst))
            (void))))  
  (add-register)
  (let ((target 
         (get-register machine (assign-reg-name inst)))  ;取得n
        (value-exp (assign-value-exp inst))) ; (cddr inst),相当于 ((reg b))
    (let ((value-proc
           (if (operation-exp? value-exp)  ; value-exp 形如 ((op rem))则是 operation-exp
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))  ;(car value-exp)形如 (reg b)
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc)) ;set-contents!设置寄存器target的值为 (value-proc) 
        (advance-pc pc))))) ;程序计数器向前推进即pc.content = (cdr pc.content)




(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))


(define (advance-pc pc) ;pc里面有状态变量 content，指向指令序列里面某处  (cdr content)就指向再下一条指令。
  (set-contents! pc (mcdr (get-contents pc))))   ; get-contents: (register 'get) changed, cdr->mcdr



(define (make-swap inst machine labels operations pc);为swap指令inst生成一个可执行过程, swap指令形如： (swap a b)
  ((machine 'allocate-register) (swap-reg1 inst))
  ((machine 'allocate-register) (swap-reg2 inst))
  (let ((r1 (get-register machine (swap-reg1 inst)))
        (r2 (get-register machine (swap-reg2 inst))))
      (lambda ()                ; execution procedure for assign
        (let ((tmp (get-contents r1)))
          (set-contents! r1 (get-contents r2))
          (set-contents! r2 tmp)
          (advance-pc pc))))) ;程序计数器向前推进即pc.content = (cdr pc.content)

(define (swap-reg1 inst)
  (cadr inst))
(define (swap-reg2 inst)
  (caddr inst))



(define (make-test inst machine labels operations flag pc)
  ;test形式: (test (op <operation-name>) <input1> ... <inputn>)
  (add-reg-list machine (cddr inst))
  (let ((condition (test-condition inst))) ;inst 形如 (test (op =) (reg n) (const 1))
    (if (operation-exp? condition) ;condition 形如 ((op =) (reg n) (const 1))
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))



(define (make-branch inst machine labels flag pc) ;branch 形如 (branch (label base-case))
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


(define (make-goto inst machine labels pc) ;inst形如(goto (reg continue))或 (goto (label fact-loop))
  
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           ((machine 'allocate-register) (register-exp-reg dest))
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))


(define (make-save inst machine stack pc) ;inst形如 (save n)
  ((machine 'allocate-register) (stack-inst-reg-name inst))
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  ((machine 'allocate-register) (stack-inst-reg-name inst))
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (add-reg-list machine (cddr inst))
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


(define (make-primitive-exp exp machine labels) ;exp形如: (reg b) 或 (const 3) 或 (label thing-done)
  ;返回值一定是个过程，执行该过程，得到exp的值。如果exp是个标号，则返回该标号代表的指令序列中的位置
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels  ;lookup-label的返回值是一个指针，指向指令序列的某处
                              (label-exp-label exp))))
           (lambda () insts))) ;
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))  ;返回寄存器的值
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))  ; (operation-exp-op exp) :(cadr (car operation-exp)))  exp形如 ((op rem) (reg a) (reg b))
        ;op是个可执行过程 operations 也许形如 ((rem #remainder) ...)
        (aprocs
         (map (lambda (e)  ;e形如: (reg a) (const 3) (lable done)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp) ;exp形如 ((op rem) (reg a) (reg b))
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
   (list (list 'rem remainder) (list '= =))

   '(test-b
       (swap a b)
       (swap a b)
     
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))


(define fib-machine-text
  '(make-machine
   (list (list 'rem remainder) (list '= =) (list '< <) (list '+ +) (list '- -))
'(fib-start
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (assign n (reg val))               ; n now contains Fib(n - 2)
   (restore val)                      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)))




(define (get-op-table lst)
  (define (change op) ;op形如 ('= =)
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
  (define op-table (get-op-table (cdr (cadr machine-text))))
  (define controller (cadr (caddr machine-text)))
  (make-machine op-table controller))


(define (run-machine machine input output) ;input 形如: ((n 7) (a 4) (b 3)),output形如 (a b c)
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
  (if (get-error-flag machine)
      (void)
      (begin 
        (init-machine machine input)
        (start machine)
        (output-machine machine output)
        (newline))))
  


(define (process-loop)
  (let ((machine '*unassigned*))
    (define (inner-loop)
      (let ((m-txt (read)))
            (if (eq? m-txt eof)
                (void)
                (if (eq? (car m-txt) 'make-machine)
                    (begin (set! machine (make-machine-from-text m-txt))
                           (display "a new machine") (newline)
                           (if (get-error-flag machine)
                               (begin (display "label error in machine") (newline))
                               (void))
                           (run-machine machine (read) (read))
                           (inner-loop))
                    (begin (run-machine machine m-txt (read)) 
                           (inner-loop))))))
    (inner-loop)))

(process-loop)

(define test-machine-text
  '(make-machine
   (list (list 'rem remainder) (list '= =) (list 'eq eq)) ;key
   '(  (assign a (const 112))
       (assign b (reg m)) ;key
       (assign b (const 168))
       (swap a b)
       (swap b a)
       (swap a1 b1)
       (test (op eq) (reg x) (reg y))
     test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op eq) (reg a) (reg k)) ;key
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

;(define test-machine (make-machine-from-text test-machine-text))
;(test-machine 'registers)
;(start test-machine)
;(get-register-contents test-machine 'a)
;(get-register-contents test-machine 'm)
;(get-register-contents test-machine 'k)
;(get-register-contents test-machine 'x)
;(get-register-contents test-machine 'y)
;(get-register-contents test-machine 'a1)
;(get-register-contents test-machine 'b1)


;(define fib-machine (make-machine-from-text fib-machine-text))
;(fib-machine 'registers)
;(get-register-contents fib-machine 'val)
;(get-register-contents fib-machine 'continue)

;(define gcd-machine (make-machine-from-text gcd-machine-text)) 
;(gcd-machine 'registers)
;(run-machine fib-machine '((n 12)) '(val))  
;(run-machine fib-machine '((n 8)) '(val))  ;
;(run-machine gcd-machine '((a 12) (b 18)) '(a))  
;(run-machine gcd-machine '((a 102) (b 27)) '(a))  


