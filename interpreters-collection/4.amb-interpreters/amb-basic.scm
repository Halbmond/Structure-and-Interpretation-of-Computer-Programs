#lang racket
;basic amb evaluator
(require racket/mpair)

(define (my-display x) 
  (cond ((and (mpair? x) (eq? (mcar x) 'primitive))
         (display (get-list-head x 2)))
        ((and (pair? x) (eq? (car x) 'procedure))
         (display (get-list-head x 3)))
        (else (display x))))
(define (my-newline) (newline))

(define (simple-proc-obj proc-obj)
  (if (mpair? proc-obj) 

      (get-list-head proc-obj 2)
      (get-list-head proc-obj 3)))

(define (get-list-head lst n) 

  (if (= n 0)
      '()
      (if (mpair? lst)
          (mcons (mcar lst) (get-list-head (mcdr lst) (- n 1)))
          (cons (car lst) (get-list-head (cdr lst) (- n 1))))))

(define (get-mlist-head lst n) 

  (if (= n 0)
      '()
      (mcons (mcar lst) (get-mlist-head (mcdr lst) (- n 1)))))


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
               
      


(define mcadr (lambda (x) (mcar (mcdr x))))
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)



(define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))
(define (mtagged-list? exp tag) 

  (if (mpair? exp) 

      (eq? (mcar exp) tag)
      false))


(define (tagged-list? exp tag) 

  (if (pair? exp) 

      (eq? (car exp) tag)
      false))



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
  (list 'if predicate consequent alternative))


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

(define (procedure-enviroment p)
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

  (if (= (mlength vars) (mlength vals))

       (mcons (make-frame vars vals) base-env) 

       (if (< (mlength vars) (mlength vals))
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

(define (apply-primitive-procedure proc args) 
   (apply  
    (primitive-implementation proc) (mymlist->list args)))  



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
        (mlist 'length  length)
        (mlist 'sqrt  sqrt)
        (mlist 'list  list)
        (mlist 'symbol? symbol?)
        (mlist 'eq? eq?)
        (mlist 'cadr cadr)
        (mlist 'append append)
        (mlist 'display  display)
        (mlist 'newline  newline)
        (mlist 'not not)
        (mlist 'void void)
        (mlist 'my-square  my-square)        
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

(define (prompt-for-input string)
   (newline) (newline) (display string) (newline))

(define (announce-output string)
   (newline) (display string) (newline))
             
(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (display object)))

 
(define (ambeval exp env succeed fail)  
  ((analyze exp) env succeed fail))  


(define (analyze exp)   
   (cond ((self-evaluating? exp) 
          (analyze-self-evaluating exp))
         ((null? exp) (lambda (env succeed fail) (succeed ((void)) fail))) 

         ((quoted? exp) (analyze-quoted exp))
         ((variable? exp) (analyze-variable exp))  

         ((assignment? exp) (analyze-assignment exp))
         ((definition? exp) (analyze-definition exp))
         ((if? exp) (analyze-if exp))
         ((lambda? exp) (analyze-lambda exp))
         ((begin? exp) (analyze-sequence (begin-actions exp)))
         ((cond? exp) (analyze (cond->if exp)))  

         ((let? exp) (analyze (let->combination exp))) 

         ((amb? exp) (analyze-amb exp))
         ((application? exp) (analyze-application exp))
         (else
          (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-self-evaluating exp) 
   (lambda (env succeed fail) 
     (succeed exp fail)))


(define (analyze-quoted exp)
   (let ((qval (text-of-quotation exp)))
     (lambda (env succeed fail) 
       (succeed qval fail))))

(define (analyze-variable exp)  
  (lambda (env succeed fail) 
     (succeed (lookup-variable-value exp env) fail)
    ))


(define (analyze-lambda exp) 
   (let ((vars (lambda-parameters exp))
         (bproc (analyze-sequence (lambda-body exp))))  
     (lambda (env succeed fail) 
       (succeed (make-procedure vars bproc env) fail)
       ))) 


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail) 
      (pproc env  
             (lambda (pred-value fail2)  
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2) 
               (define-variable! var val env)
               (succeed (void) fail2)
               ) 
             fail))))  

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        

               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed (void) 

                          (lambda ()    

                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))  


(define (analyze-sequence exps) 
  (define (sequentially a b) 
    (lambda (env succeed fail) 
      (a env
         (lambda (a-value fail2)  
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (void))
    (loop (car procs) (cdr procs))))

  
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail) 

      (fproc env  

             (lambda (proc fail2)  

               (get-args aprocs
                         env
                         (lambda (args fail3) 

                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))


(define (get-args aprocs env scd fail) 
  (if (null? aprocs)
      (scd '() fail)  
      ((car aprocs) env   
                    (lambda (arg fail2) 
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3) 

                                  (scd (cons arg args)  
                                           fail3))
                                fail2))
                    fail)))


(define (execute-application proc args succeed fail) 
  (cond ((primitive-procedure? proc)
         (let ((m (apply-primitive-procedure proc args)))
           (succeed m fail)
         ))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              (list->mlist args) 

                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))  

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))


(define (analyze-amb exp)  
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail) 
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed 
                           (lambda () 

                             (try-next (cdr choices))))))
      (try-next cprocs))))  

  
(define (procedure-environment proc)
  (car (cdr (cdr (cdr proc)))))


(define rq '(define (require p)
  (if (not p)
      (amb)
      (void))))
  


(define glb-succeed 
  (lambda (val next)
    (display "succeed,val = " ) (display val) (newline)
    ))

(define glb-fail
  (lambda () 
    (display "no answer") (newline)))

(define glb-env (setup-environment))
(ambeval rq glb-env (lambda (val fail) (void)) glb-fail)


(define (my-driver-loop)
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (begin (ambeval input glb-env 
                        (lambda (val fail)
                          (if (eq? val (void))
                              (void)
                              (begin (display val) (newline))))
                          glb-fail)
               (my-driver-loop)))))
(my-driver-loop)
