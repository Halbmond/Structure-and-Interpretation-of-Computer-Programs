#lang racket
(require racket/mpair)
;(define my-display display)
;(define my-newline newline)
(define my-display (lambda (x) (void)))
(define my-newline (lambda () (void)))

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

;(define (list->mlist x) 
;  (if (null? x)
;      (mlist )
;      (mcons (car x) (list->mlist (cdr x)))))

;-----------------------------expressions:
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
;(tagged-list? '(define p 1)  'define) => #t

(define (variable? exp) (symbol? exp))  ;(variable? 's) => #t  (variable? '(s)) => #f

(define (quoted? exp)  (tagged-list? exp 'quote))

(define (text-of-quotation exp)  (cadr exp))

(define (assignment? exp)  (tagged-list? exp 'set!))

(define (assignment-variable exp)  (cadr exp))

(define (assignment-value exp)  (caddr exp))

(define (definition? exp)  (tagged-list? exp 'define))



(define (let? exp)  (tagged-list? exp 'let))
(define (let-body exp)  (cddr exp)) ;
(define (let-clauses exp)  (cadr exp))
(define (let->combination exp)
  (cons (make-lambda (map car (let-clauses exp)) ; used to make the mistake of using list instead of con
               (let-body exp)) (map cadr (let-clauses exp))))

(define (definition-variable exp)
  (if (variable? (cadr exp)) ;(define p 2) => (cadr exp) is variable . (define (p x ) ..) => (cadr exp) is a pair
             (cadr exp)
             (caadr exp))) ;if the second item of exp is a pair, caadr get the head of that pair

(define (definition-value exp) ;the result may be just a list, not evaluated yet
  (if (symbol? (cadr exp)) ; (define p ...)
      (caddr exp) ; caddr get the third item of a list. the third item may be a aotom ,or a list
      (make-lambda (cdadr exp)             ; cdadr means (cd cadr)  this is to get the param list . ;(cdadr (list 1 (list 2 3 4) 5)) => '(3 4)
                   (cddr exp)))) ;may be there is this situation: (define (p4 x) (+ x 1) (display "tmp") (newline) (* x 2))
 ; for (define (p x) (+ x 1)),  (cddr exp) is a list which contains one element ,and that element is a list (+ x 1)

;(definition-value '(define (p x) ( + x 1))) => (lambda (x) (+ x 1))


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) ; parameters and body are all lists. parameters could be '(x y z),or '(x), never be x
                                      ;body could be '((+ x 1)), never be '(+ x 1)  
  (cons 'lambda (cons parameters body)))  ;the result is '(lambda 

;(cons '(x) '(+ x 1)) => '((x) + x 1)
;(cons '(x) '((+ x 1))) => '((x) (+ x 1))




;the comment below uses (if (> x 0) (+ x 1) (- x 1)) as sample
(define (if? exp) (tagged-list? exp 'if)) 
(define (if-predicate exp) (cadr exp)) ;  result is '(> x 0) , not '((> x 0))
(define (if-consequent exp) (caddr exp)) ;result is  '(+ x 1), not '((+ x 1))
(define (if-alternative exp)
  (if (null? (cdddr exp))
             'false
             (cadddr exp))) ;get the forth element of exp  result  '(- x 1),not '((- x 1))
(define (make-if predicate consequent alternative)
  (if (null? consequent)
      (list 'if predicate predicate alternative)
      (list 'if predicate consequent alternative)))


(define (begin? exp)
  (tagged-list? exp 'begin))
;the comment below use (begin (+ x 1) (- x 2)) as sample
(define (begin-actions exp) (cdr exp)) ; '((+ x 1) (- x 2)) the result must be a list. 
;seq is the actions in begin expression, seq is a list
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq)
  (cons 'begin seq)) ;result is '(begin (+ x 1) (- x2))


;change  '((+ x 1) (+ x 2) (+ x 3)...)  into '(begin (+ x1) (+ x 2) (+ x 3)...)
;change '((+ x 1)) into '(+ x 1)
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

;cond-> if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ;the list of clauses
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause)) ;((> x 1) (+ x 1) (- x 1)) => ((= x 0) (* x 2))) action could be multi items
;((> x 1) (+ x 2)) => ((+ x 2))



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


;(cond->if '(cond ((= x 0) 0)
;                 ((> x 0) x)
;                 ((< x 0) -x)
;                 (else (* 2 x))))
;=> '(if (= x 0) 0 (if (> x 0) x (if (< x 0) -x (* 2 x))))



(define (eval exp env)
;  (my-display "-------in eval, exp=:") (my-display exp) (my-newline)
;  (my-display "-------in eval, env=:") (my-display env) (my-newline)   
  (cond ((self-evaluating? exp ) exp)
        ((null? exp) (void))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (cdr exp) env))
        ((or? exp) (eval-or (cdr exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env)) ; see let->combination  in 4.6
        
        ((application? exp)
;old         (my-apply (eval (operator exp) env)  
;                (list-of-values (operands exp) env)))
         (my-apply (actual-value (operator exp) env) ;changed for delay
                   (operands exp)
                   env))
        (else
         (error "unknown expression type -- EVAL" exp))))

 
(define (delay-it bmem exp env) ;bmem = true means rememberable thunk
  (if (true? bmem)
      (mlist 'mem-thunk exp env)
      (mlist 'thunk exp env)))

(define (thunk? obj)
  (or (mtagged-list? obj 'thunk) (mtagged-list? obj 'mem-thunk)))
(define (mem-thunk? obj)  
  (mtagged-list? obj 'mem-thunk))

(define (thunk-exp thunk)
  (mcar (mcdr thunk)))

(define (thunk-env thunk)
  (mcar (mcdr (mcdr thunk))))


(define (evaluated-thunk? obj)
  (mtagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))


;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))

(define (force-it obj)
  (cond ((thunk? obj)
         (if (mem-thunk? obj)
             (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
               (set-mcar! obj 'evaluated-thunk)
               (set-mcar! (mcdr obj) result) ;replace exp with its value
               (set-mcdr! (mcdr obj) '())
               result)
             (actual-value (thunk-exp obj) (thunk-env obj))))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))
                                   
(define (actual-value exp env)
  (force-it (eval exp env)))  ;(eval exp env)  maybe a thunk


(define (my-apply procedure arguments env)  ;every elemnt in arguments is in the original form,not evaled. parament env added
  ;(my-display "-------in my-apply,procedure = ") (my-display procedure) (my-display "  argumets= " )(my-display arguments) (my-newline) 
  
  (cond ((primitive-procedure? procedure)
         (my-display "-------in my-apply,primitive, procedure = ") (my-display procedure) (my-display "  argumets= " )(my-display arguments) (my-newline) 
         (let ((tmp (list-of-arg-values arguments env)))
           (if (eq? (primitive-implementation procedure) +)
               (begin (my-display "********in primitive, arguments=" ) (my-display arguments) (my-newline) (my-display "********tmp=") (my-display tmp) (my-newline))
               (void))
           (apply (primitive-implementation procedure) tmp))) ;changed for delay base proc para is strict
        
        ((compound-procedure? procedure)
         (my-display "-------in my-apply,compound, procedure = ") (my-display procedure) (my-display "  argumets= " )(my-display arguments) (my-newline) 
         (eval-sequence
          (procedure-body procedure)
          (extend-environment  
           (procedure-parameters-adv procedure)
           (list->mlist (adv-list-of-args (procedure-parameters procedure) arguments env))
           (procedure-enviroment procedure)))
          ;(display (adv-list-of-args (procedure-parameters procedure) arguments env)) (newline) ;addfor debug
         
         )
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env) 
  ;(display "in list-of-arg-valus,exp = ") (display exps) (newline)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                               env))))
     
(define (list-of-delayed-args exps env) 
  
  (if (no-operands? exps)
      '()
      (cons (delay-it true (first-operand exps) env) ;modifor adv
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (adv-list-of-args ori-para exps env) 
  (if (no-operands? exps)
      '()
      (if (pair? (mcar ori-para))
          (if (eq? (cadr (mcar ori-para)) 'lazy-memo)
              (cons (delay-it true (first-operand exps) env)
                (adv-list-of-args (mcdr ori-para) (rest-operands exps) env)) 
              (cons (delay-it false (first-operand exps) env)
                (adv-list-of-args (mcdr ori-para) (rest-operands exps) env))) 
          (cons (actual-value (first-operand exps) env)
                (adv-list-of-args (mcdr ori-para) (rest-operands exps) env)))))

(define (list-of-values exps env)  
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)  ;changed for delay
  (if (true? (actual-value (if-predicate exp) env))
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
  ;(my-display "-------in eval-sequence, exps= ") (my-display exps) (my-newline);changed
  ;(my-display "-------in eval-sequence,env= ") (my-display env) (my-newline);changed
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps )env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
 (void))

(define (eval-definition exp env)
  ;(my-display "-------in eval-definition,exp = ") (my-display exp) (my-newline)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) ;definition-value
    env)  
    (void))

;----------------- environment p261
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env)) 



(define (compound-procedure? p)  ; (primitive #<procedure:car>)  (procedure (xx yy) ((* xx yy 5)) #0#)   #0#
  (tagged-list? p 'procedure))

;old 
(define (procedure-parameters p) 
  (list->mlist (cadr p))) ;changed . ori : (cadr p)

;addfor adv
(define (procedure-parameters-adv p) 
  (define (aid x)
    (if (pair? x)
        (car x)
        x))
  (list->mlist (map aid (cadr p))))
  
(define (procedure-body p)
  (caddr p))

(define (procedure-enviroment p)
  (cadddr p))


(define (enclosing-environment env) (mcdr env)) 

(define (first-frame env) (mcar env)) ;changed , maybe not necessary
(define the-empty-environment (mlist )) ;changed

(define (make-frame variables values) 
  (mcons variables values)) ;changed; ori: cons 

(define (frame-variables frame ) (mcar frame)) ;changed  
(define (frame-values frame) (mcdr frame))  

(define (add-binding-to-frame! var val frame)  
  (set-car! frame (mcons var (mcar frame))) 
  (set-cdr! frame (mcons val (mcdr frame))))  ;changed   

(define (extend-environment vars vals base-env)  
  ;(my-display "-------in extend-environment:vars and vals :" ) (my-display vars) (my-display " vals: ") (my-display vals) (my-newline)
  ;(my-display "-------in extend-environment:base-env:" ) (my-display base-env)  (my-newline)
  (if (= (mlength vars) (mlength vals));changed
      (begin  
      
      ;(my-display "-------in extend-env, result: ") (my-display (mcons (make-frame vars vals) base-env)) (my-newline)
      (mcons (make-frame vars vals) base-env)) ;changed to mcons,maybe not necessary
      (if (< (mlength vars) (mlength vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars "   *********  " vals))))

(define (lookup-variable-value var env)
  ;(my-display "-------in lookup-variable-value:") (my-display var) (my-newline)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) ;changed
              (mcar vals)) ;changed for delay
            (else (scan (mcdr vars) (mcdr vals))))) ;changed
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  ;(display "   ---------lookup-vairable-value:") (display var) (newline) ;changed
  (env-loop env))



(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  ;changed
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))  ;changed
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) 
  ;(my-display "-------in define-variable! var =") (my-display var) (my-display "  val=") (my-display val) (my-newline)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) ;changed
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals))))) ;changed
    (scan (frame-variables frame)  
          (frame-values frame))))

;p264
(define (apply-primitive-procedure proc args) ;proc : (primitive #<procedure:car>) 
   (apply  ;changed , ori   apply-in-underlying-scheme ****
    (primitive-implementation proc) args))  

;basic functions
(define (my-square x ) (* x x))

(define primitive-procedures
  (mlist (mlist 'car car) ;changed
        (mlist 'cdr cdr) ;changed
        (mlist 'cons cons) ;changed
        (mlist 'null? null?) ;changed
        (mlist '+ +) ;changed
        (mlist '* *) ;changed
        (mlist '- -) ;changed
        (mlist '/ /) ;changed
        (mlist '< <) ;changed
        (mlist '> >) ;changed
        (mlist '= =) ;changed
        (mlist 'number? number?) ;changed
        (mlist 'pair? pair?) ;changed
        (mlist 'not not) ;changed
        (mlist 'remainder remainder) ;changed
        (mlist 'my-square  my-square)
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


(define (primitive-procedure-names) ;a mlist ,and the content is '(car cdr cons null? + * - / < > = my-square)
  (mmap mcar  ;changed
       primitive-procedures))  ; primitive-procedures : (('car car) ('cdr cdr) ('cons cons) ('null? null?) ('+ +))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc))) ;changed
       primitive-procedures))


(define (setup-environment ) 
  (let ((initial-env
         (extend-environment (primitive-procedure-names) ;primitive-procedure-names is a mlist ,and the content is '(car cdr cons null? + * - / < > = my-square)
                             (primitive-procedure-objects)
                             the-empty-environment))) 
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))


        
(define (primitive-procedure? proc)  
        (mtagged-list? proc 'primitive)) ;chanaged

(define (primitive-implementation proc) (mcadr proc)) ;changed



(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value:")

(define (driver-loop) ;;changed for delay
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (let ((output (actual-value input glb-env)));changed for delay
          ;(announce-output output-prompt)
          ;(display "*************done" ) (newline)
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

(define glb-env (setup-environment))
(driver-loop)
