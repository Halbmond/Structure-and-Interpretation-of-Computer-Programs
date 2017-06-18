#lang racket

;basic analyse and excution splited scheme interpreter in text book, with "let", "and", "or" implemented,

;this program support this:
;(cond ((and (> 5 3) (> 6 2) (* 3 4))))
;=> 12


(require racket/mpair)

(define (my-display x)   (void))
(define my-newline void)

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


(define (analyze-if exp)
   (let ((pproc (analyze (if-predicate exp)))  
         (cproc (analyze (if-consequent exp))) 
         (aproc (analyze (if-alternative exp)))) 
     (lambda (env)
       (if (true? (pproc env))
           (cproc env)
           (aproc env)))))


(define (and? exp)
  (tagged-list? exp 'and))



(define (analyze-and exp)
  (define (helper result ops env)
    (if (null? ops)
        result
        (let ((tmp ((car ops) env)))
          (if (true? tmp)
              (helper tmp (cdr ops) env)
              false))))
  (let ((procs (map analyze exp)))
    (lambda (env) (helper true procs env))))

(define (or? exp)
  (tagged-list? exp 'or))

(define (analyze-or exp)
  (define (helper ops env)
    (if (null? ops)
        false
        (let ((tmp ((car ops) env)))
          (if (true? tmp)
              tmp
              (helper (cdr ops) env)))))
  (let ((procs (map analyze exp)))
    (lambda (env) (helper procs env))))
  
  
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

(define (driver-loop)
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (let ((output (eval input glb-env)))
          (user-print output)
          (driver-loop)))))


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
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))


             
(define (eval exp env)
   ((analyze exp) env)) 

(define (analyze exp)   
   (cond 
         ((null? exp) (lambda (env) (void))) 
         ((self-evaluating? exp) (analyze-self-evaluating exp))
         ((quoted? exp) (analyze-quoted exp))
         ((variable? exp) (analyze-variable exp))  
         ((assignment? exp) (analyze-assignment exp))
         ((definition? exp) (analyze-definition exp))
         ((if? exp) (analyze-if exp))
         ((and? exp) (analyze-and (cdr exp)))
         ((or? exp) (analyze-or (cdr exp)))
         ((lambda? exp) (analyze-lambda exp))
         ((begin? exp) (analyze-sequence (begin-actions exp)))
         ((cond? exp)  (analyze (cond->if exp)))  
         ((let? exp) (analyze (let->combination exp))) 
         ((application? exp) (analyze-application exp))
         (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
   (lambda (env) exp))
(define (analyze-quoted exp)
   (let ((qval (text-of-quotation exp)))
     (lambda (env) qval)))

(define (analyze-variable exp)  
   (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
   (let ((var (assignment-variable exp))
         (vproc (analyze (assignment-value exp))))
     (lambda (env)
       (set-variable-value! var (vproc env) env)
       (void))))
 (define (analyze-definition exp)
   (let ((var (definition-variable exp))
         (vproc (analyze (definition-value exp)))) 

     (lambda (env)
       (define-variable! var (vproc env) env)
       (void))))


(define (analyze-lambda exp)
   (let ((vars (lambda-parameters exp))
         (bproc (analyze-sequence (lambda-body exp))))  
     (lambda (env) (make-procedure vars bproc env)))) 

(define (analyze-sequence exps) 
   (define (sequentially proc1 proc2)
     (lambda (env) (proc1 env) (proc2 env)))
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

(define (analyze-sequence_bad exps)
  (lambda (env)
    (define (exec-procs procs)
      (let ((proc (analyze (car procs))))
      (if (null? (cdr procs))
          (proc env)
          (begin (proc env) (exec-procs (cdr procs))))))
    (exec-procs exps)))


(define (analyze-application exp)  
  (let ((fproc (analyze (operator exp))) 

         (aprocs (map analyze (operands exp))))   

     (lambda (env)
       (execute-application (fproc env)
                            (map (lambda (aproc) (aproc env)) 
                                 aprocs)))))
(define (execute-application proc args)  
   (cond ((primitive-procedure? proc) 
          (apply (primitive-implementation proc) (mymlist->list args)))
         ((compound-procedure? proc) 
          ((procedure-body proc)  
           (extend-environment (procedure-parameters proc) 
                               (list->mlist args) 
                               (procedure-environment proc))))
         (else
          (error
           "Unknown procedure type -- EXECUTE-APPLICATION"
           proc))))
 

(define (procedure-environment proc)
  (car (cdr (cdr (cdr proc)))))


(define glb-env (setup-environment))
(driver-loop)


