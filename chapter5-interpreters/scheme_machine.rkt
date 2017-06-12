#lang racket
(require racket/mpair)
;a machine that can interpret scheme programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;****your code starts here
;;;;;;;;;;;;;;;;;;;;;****common functions
(define (mtagged-list? exp tag) 
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))

(define (tagged-list? exp tag) 
  (if (pair? exp) 
      (eq? (car exp) tag)
      false))

(define (mylist->mlist lst) 
  (if (null? lst)
      '()
      (if (pair? lst)
          (let ((first (car lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (cdr lst)))
                (mcons first (mylist->mlist (cdr lst)))))
          (let ((first (mcar lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (mcdr lst)))
                (mcons first (mylist->mlist (mcdr lst))))))))

(define (mymlist->list mlst)
  (if (null? mlst)
      '()
      (if (mpair? mlst)
          (let ((first (mcar mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (mcdr mlst)))
                (cons first (mymlist->list (mcdr mlst)))))
          (let ((first (car mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (cdr mlst)))
                (cons first (mymlist->list (cdr mlst))))))))
              
(define mcadr (lambda (x) (mcar (mcdr x))))
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)
;;;;;;;;;;;;;;;;;;;;;;program of basic scheme interpreter

(define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))


(define (variable? exp) (symbol? exp))  

(define (quoted? exp)  (tagged-list? exp 'quote))

(define (text-of-quotation exp)  (cadr exp))

(define (assignment? exp)  (tagged-list? exp 'set!))

(define (assignment-variable exp)  (cadr exp))

(define (assignment-value exp)  (caddr exp))

(define (definition? exp)  (tagged-list? exp 'define))



(define (let? exp)  (tagged-list? exp 'let))
(define (let-body exp)  (cddr exp)) 

(define (let-clauses exp)  (cadr exp))
(define (let->combination exp)
  (cons (make-lambda (map car (let-clauses exp)) 
               (let-body exp)) (map cadr (let-clauses exp))))

(define (definition-variable exp)
  (if (variable? (cadr exp)) 
             (cadr exp)
             (caadr exp))) 


(define (definition-value exp) 
  (if (symbol? (cadr exp)) 
      (caddr exp) 
      (make-lambda (cdadr exp)             
                   (cddr exp)))) 


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) 
   (cons 'lambda (cons parameters body)))  


(define (if? exp) (tagged-list? exp 'if)) 
(define (if-predicate exp) (cadr exp)) 

(define (if-consequent exp) (caddr exp)) 

(define (if-alternative exp)
  (if (null? (cdddr exp))
             'false
             (cadddr exp))) 

(define (make-if predicate consequent alternative)
  (if (null? consequent)
      (list 'if predicate predicate alternative)
      (list 'if predicate consequent alternative)))


(define (begin? exp)
  (tagged-list? exp 'begin))


(define (begin-actions exp) (cdr exp)) 

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq)
  (cons 'begin seq)) 

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq)) 
        (else (make-begin seq))))


(define (application? exp) (pair? exp))  
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops)) 

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) 

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause)) 

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (list-of-values exps env)  
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and operands env)
  (define (helper result ops env)
    (if (null? ops)
      result
      (let ((tmp (eval (car ops) env)))
        (if (true? tmp)
          (helper tmp (cdr ops) env)
          false))))
  (helper true operands env))


(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or operands env)
  (if (null? operands)
      false
      (if (true? (eval (car operands) env))
      true
      (eval-or (cdr operands) env))))
      
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps )env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
 (void))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) 
    env)  
    (void))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))  

(define (compound-procedure? p)  
  (tagged-list? p 'procedure))

(define (procedure-parameters p) 
  (list->mlist (cadr p))) 


(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

(define (enclosing-environment env) (mcdr env)) 
(define (first-frame env) (mcar env)) 
(define the-empty-environment (mlist )) 
(define (make-frame variables values) 
  (mcons variables values)) 


(define (frame-variables frame ) (mcar frame)) 

(define (frame-values frame) (mcdr frame))  


(define (add-binding-to-frame! var val frame)  
  (set-car! frame (mcons var (mcar frame)))  
  (set-cdr! frame (mcons val (mcdr frame))))  


(define (extend-environment vars vals base-env)  
  (if (not (mpair? vals))
      (set! vals (list->mlist vals))
      (void))  ;addfor scheme-machine by guo wei
  (if (= (mlength vars) (mlength vals))
      (begin  
      (mcons (make-frame vars vals) base-env)) 
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) 
             (mcar vals)) 
            (else (scan (mcdr vars) (mcdr vals))))) 
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



(define (set-variable-value! var val env) 
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))  
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) 
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) 

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals))))) 
    (scan (frame-variables frame)  
          (frame-values frame))))


(define (my-square x ) (* x x))

(define primitive-procedures
  (mlist (mlist 'car car) 
        (mlist 'cdr cdr) 
        (mlist 'cons cons) 
        (mlist 'null? null?) 
        (mlist '+ +) 
        (mlist '* *) 
        (mlist '- -) 
        (mlist '/ /) 
        (mlist '< <) 
        (mlist '> >) 
        (mlist '= =) 
        (mlist 'number? number?) 
        (mlist 'pair? pair?) 
        (mlist 'not not) 
        (mlist 'remainder remainder) 
        (mlist 'my-square  my-square)
        (mlist 'length  length)
        (mlist 'sqrt  sqrt)
        (mlist 'list  list)
        (mlist 'symbol? symbol?)
        (mlist 'eq? eq?)
        (mlist 'cadr cadr)
        (mlist 'append append)
        )) 

(define primitive-procedures2
  (mlist
        (mlist '* *) 
        )) 

(define (primitive-procedure-names) 
  (mmap mcar  
       primitive-procedures))  

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc))) 
       primitive-procedures))


(define (setup-environment ) 
  (let ((initial-env
         (extend-environment (primitive-procedure-names) 
                             (primitive-procedure-objects)
                             the-empty-environment))) 
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))


        
(define (primitive-procedure? proc)  
        (mtagged-list? proc 'primitive)) 


(define (primitive-implementation proc) (mcadr proc)) 

(define glb-env (setup-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;**** function added for machine interpreter

(define (eof? x)
  (if (eq? x eof)
      true
      false))  ;add by guo wei

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))
(define (get-global-environment)
  glb-env)

(define (user-print x)
  (if (eq? x (void))
      (void)
      (displayln x)))

(define (apply-primitive-procedure op args)
  (apply (primitive-implementation op) args))



(define eceval-operations 
           (list
                (list 'rem remainder)                                  
                (list 'self-evaluating? self-evaluating?)
		(list 'variable? variable?)
		(list 'quoted? quoted?)
		(list 'assignment? assignment?)
		(list 'definition? definition?)
		(list 'if? if?)
		(list 'lambda? lambda?)
		(list 'begin? begin?)
		(list 'application? application?)
		(list 'lookup-variable-value lookup-variable-value)
		(list 'text-of-quotation text-of-quotation)
		(list 'lambda-parameters lambda-parameters)
		(list 'lambda-body lambda-body)
		(list 'make-procedure make-procedure)
		(list 'operands operands)
		(list 'operator operator)
		(list 'empty-arglist empty-arglist)
		(list 'no-operands? no-operands?)
		(list 'first-operand first-operand)
		(list 'last-operand? last-operand?)
		(list 'adjoin-arg adjoin-arg)
		(list 'rest-operands rest-operands)
		(list 'primitive-procedure? primitive-procedure?)
		(list 'compound-procedure? compound-procedure?)
		(list 'apply-primitive-procedure apply-primitive-procedure)
		(list 'procedure-parameters procedure-parameters)
		(list 'procedure-environment procedure-environment)
		(list 'procedure-body procedure-body)
		(list 'extend-environment extend-environment)
		(list 'begin-actions begin-actions)
		(list 'first-exp first-exp)
		(list 'last-exp? last-exp?)
		(list 'rest-exps rest-exps)
		(list 'no-more-exps? no-more-exps?)
		(list 'if-predicate if-predicate)
		(list 'true? true?)
		(list 'if-alternative if-alternative)
		(list 'if-consequent if-consequent)
		(list 'assignment-variable assignment-variable)
		(list 'assignment-value assignment-value)
		(list 'set-variable-value! set-variable-value!)
		(list 'definition-variable definition-variable)
		(list 'definition-value definition-value)
		(list 'define-variable! define-variable!)
                ;(list 'prompt-for-input prompt-for-input)
		(list 'read read)
		(list 'get-global-environment get-global-environment)
		;(list 'announce-output announce-output)
		(list 'user-print user-print)
                (list '+ +) ;no use ,just for debug
                (list 'eof? eof?) ;add by guo wei
                )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;****basic machine interpreter 




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
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
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
           (lambda () 
             (if (eq? c 'ok)
                 (void)
                 c))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;****your code ends here
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
               (lambda (seq) 
                 (set! the-instruction-sequence seq)
                 ))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) 

               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack) 

              ((eq? message 'operations) the-ops) 
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))



(define (make-machine register-names ops controller-text);
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (start machine)
  (machine 'start))

;;;;;;;;;;;;;;;;;;;;;;;;;;****scheme machine controler


(define scheme-machine-controller 
'(

read-eval-print-loop
  (perform (op initialize-stack))
  ;(perform
   ;(op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))  ;before doing somthing that may change the return address, always assign continue with right label
  (goto (label eval-dispatch))
print-result
;  (perform
;   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val)) ;the value of exp is stored in val
  (goto (label read-eval-print-loop))
  
  
  
eval-dispatch
;after this is completed, the value of exp is stored in reg val,and 
;program goto the address stored in reg continue;
; eval value of exp in env
  (test (op eof?) (reg exp)) ;addby guo wei
  (branch (label program-end))
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))  ;assign exp to val and then goto continue
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))
  
ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))
;just store the value of exp in reg val,and goto continue
;the value of a lambda is a function object, and it is stored also in val  

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))  ; evaluate the assignment value stored in exp
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env)) ;variable name is stored in uenv
  (assign val (const ok))
  (goto (reg continue))
  
 
  
  
ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))  ; evaluate the definition value
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
  
  
ev-if
  (save exp)                    ; save the whole if expression for later
  (save env)
  (save continue) ;after ev-if,should goto continue
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))  ; evaluate the predicate  
  
  
ev-if-decide
  (restore continue) ;the address to which we should go after the whole ev-if is done
  (restore env)  ;the env for the whole if exp
  (restore exp)  ;the whole if exp
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))

ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))  

  

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))
  
ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-end
  (restore continue)
  (goto (reg continue))  
  
  
  
  
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))


ev-appl-did-operator
  (restore unev)                  ; the operands
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))         ; the operator,is a function object
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)


ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev) ;the oprands, in which the first one is going to be evaluated and can be discarded
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)  ;operands, in which the fisrt one is already evaluated
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))



apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))





primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))




compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))
                   
  
unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))
unknown-procedure-type
  (restore continue)    ; clean up stack (from apply-dispatch)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))
signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
program-end ;addfor scheme-machine by guo wei

)
)  ;end of scheme-machine-controller
  


;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scheme-machine
  (make-machine
   '(exp env val proc argl continue unev c d)
   eceval-operations
   scheme-machine-controller
  ))
(start scheme-machine)