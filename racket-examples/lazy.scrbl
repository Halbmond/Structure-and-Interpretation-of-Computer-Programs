#lang scribble/manual

@(require scribblings/reference/mz)

@title{Lazy Racket}

@codeblock{
#lang lazy
;; An infinite list:
(define fibs
  (list* 1 1 (map + fibs (cdr fibs))))
 
;; Print the 1000th Fibonacci number:
(print (list-ref fibs 1000))
}

In this program, we use @racket[Lazy Racket] to define an "infinite" fibonacci
sequence, and to print its 1001st element.

The first line of this program specifies that we are using the 
@racket[Lazy Racket] language.
@margin-note*{ Once an expression is evaluated, its value is cached and the 
              expression need not be evaluated again }
In @racket[Lazy Racket], expressions are only evaluated when their results 
are needed.

Then, we define @racket[fibs] as a lazy list with @racket[list*], 
which builds a list out of its arguments (the last argument becomes the tail 
of the list). Here we create a list whose first 2 elements are 1, and whose 
tail is defined by the expression @racket[(map + fibs (cdr fibs))]. The tail 
can refer to @racket[fibs] because it isn't immediately evaluated. This 
expression will be evaluated as needed whenever more elements of @racket[fibs] 
are accessed. This shows how laziness can be used to define cyclic data 
structures. 


@margin-note*{ Note that list indexes start at 0 }
In the last line, we access the 1001st element of @racket[fibs] with 
@racket[(list-ref fibs 1000)] and @racket[print] it.
