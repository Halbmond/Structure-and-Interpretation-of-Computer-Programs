
question:

> (define a 5)
> (symbol? a)
#f


(define-syntax Hello
(lambda (stx)
(displayln stx)
(displayln (cdr (syntax->datum stx)))
(datum->syntax stx (cdr (syntax->datum stx)))))
(Hello + 1 3)

> (for/list ([e (in-slice 3 (in-range 8))]) e)
'((0 1 2) (3 4 5) (6 7))




> (string->symbol "")
'||
> (string->symbol "str")
'str
> (string->symbol "s")
's
> (map string->symbol
       '("42" "'" "()" "(1 2 3)" "\"str ing\"" ""))
'(|42| |'| |()| |(1 2 3)| |"str ing"| ||)


;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))


 #(1 apple 3)
 reads equal to 
(vector 1 'apple 3)
 #3("apple" "banana")
 reads equal to 
(vector "apple" "banana" "banana")
 #3()
 reads equal to 
(vector 0 0 0)






> (eq? (quote ()) (list))
#t
> (eqv? (quote ()) (list))
#t
> (equal? (quote ()) (list))
#t


> (letrec (
      [fac (lambda (n) 
         (if (= 0 n)
             1
             (* n (fac (sub1 n)))))])
    (map fac (range 10)))
'(1 1 2 6 24 120 720 5040 40320 362880)


> (substring "the boy out of the country" 4 7)
"boy"


> (quote ("red" "green" "blue"))
'("red" "green" "blue")



(< 1 2 3 4 5 6 7 8 9 10) ; true

(/ 1 2 3 4 5 6 7 8 9 10) ; 1/3628800

cdr是构造一张新表，还是仅返回一个指向原表的指针？
答案：在Racket中是返回原表指针


cons会形成一个新的“盒子”


> (pair? '(1 2 3))
#t
> (pair? '(1 2 3))
#t
> (pair? '(1))
#t
> (pair? '())
#f
> (pair? 1)
#f
>

> (list? '())
#t
> (list? '(1))
#t

; (define (p) (p))
; (define (test x y)
; (if (= x 0)
;     0
;     y))
; (test 0 (p))
; 1. applicative-order:
; --> 用 actual-arguments 替换 formal-parameters:
;     (test 0 (p))
; 现在开始为各个子表达式求值, test 的值就是 (if (= x 0) 0 y), 接下来 0 的值当然就是 0 自己, 
;当 eval (p) 的值时, 根据 (define (p) (p)), 这很显然会陷入无限递归.
; 2. normal-order:
; --> 用 actual-arguments 替换 formal-parameters:
;     (test 0 (p)) 
; 接下来将 test 求值(eval), test 的值是:
; (if (= x 0) 0 y)
; 接下来是 apply(将实际参数替换 procedure <body>, 注意, 不需要 eval arguments):
; ((if (= x 0) 0 y) 0 (p))
; 替换为
; (if (= 0 0) 0 (p))
; 由于 if 是特殊规则, 它的求值顺序总是一样的:
; (if #t 0 (p))
; 0
; 所以 applicative-order 是, 先 eval 每一个 sub-expression, 然后再 apply
;
; on the other side, normal-order 则是, 先只 eval leftmost element, 
;然后直接apply/subsitute left(剩下的, rest remaining) elements 到 leftmost element 的 <body>.
; 由于 SICP 已经提及 scheme (基本上)是 applicative-order 的


;;; Comments

;; Single line comments start with a semicolon

#| Block comments
   can span multiple lines and...
    #|
       they can be nested!
    |#
|#

#|
If the reader finds no delimited . among the elements between parentheses,
then it produces a list containing the results of the recursive reads.

If the reader finds two data between the matching parentheses that are separated by a delimited .,
then it creates a pair. More generally,
if it finds two or more data where the last datum is preceded by a delimited .,
then it constructs nested pairs: the next-to-last element is paired with the last,
then the third-to-last datum is paired with that pair, and so on.

If the reader finds three or more data between the matching parentheses,
and if a pair of delimited .s surrounds any other than the first and last elements,
the result is a list containing the element surrounded by .s as the first element,
followed by the others in the read order. This convention supports a kind of infix notation at the reader level.
|#

Examples:
 ()
 reads equal to 
(list)
 (1 2 3)
 reads equal to 
(list 1 2 3)
 {1 2 3}
 reads equal to 
(list 1 2 3)
 [1 2 3]
 reads equal to 
(list 1 2 3)
 (1 (2) 3)
 reads equal to 
(list 1 (list 2) 3)
 (1 . 3)
 reads equal to 
(cons 1 3)
 (1 . (3))
 reads equal to 
(list 1 3)
 (1 . 2 . 3)
 reads equal to 
(list 2 1 3)


> '(1 2 3 . 4 . 5 6 7) ;infix expression
'(4 1 2 3 5 6 7)


Examples:
 #(1 apple 3)
 reads equal to 
(vector 1 'apple 3)
 #3("apple" "banana")
 reads equal to 
(vector "apple" "banana" "banana")
 #3()
 reads equal to 
(vector 0 0 0)



> (values 10 3)
10
3
> (quotient/remainder 10 3)
3
1
>

(define (quotient/remainder n m)
	(values (quotient n m) (remainder n m)))





(case message
	((<msg-1>) <method-1>)
	((<msg-2>) <method-2>)
	...
	((<msg-n>) <method-n>)
	(else <expr>)
)




(define (rem-dups s)
  (match s
    ['()                '()]
    [(list-rest a a p)  (rem-dups (cons a p))]
    [(list-rest a p)    (cons a (rem-dups p))]))


> (map rem-dups
       '(()
         (1)
         (1 1)
         (1 2 1)
         (1 1 1 2)
         (1 1 2 2 3 1)))
         
'(() (1) (1) (1 2 1) (1 2) (1 2 3 1))




The andmap function is actually closer to foldl than map, since andmap doesn’t produce a list. Still,
(andmap f (list x y z)) is equivalent to (and (f x) (f y) (f z))
in the same way that
(map f (list x y z)) is equivalent to (list (f x) (f y) (f z)).




> (cartesian-product '(1 2 3) '(a b c))
'((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c))
> (cartesian-product '(4 5 6) '(d e f) '(#t #f))
'((4 d #t)
  (4 d #f)
  (4 e #t)
  (4 e #f)
  (4 f #t)
  (4 f #f)
  (5 d #t)
  (5 d #f)
  (5 e #t)
  (5 e #f)
  (5 f #t)
  (5 f #f)
  (6 d #t)
  (6 d #f)
  (6 e #t)
  (6 e #f)
  (6 f #t)
  (6 f #f))
