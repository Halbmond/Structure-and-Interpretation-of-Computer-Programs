#lang scribble/manual

@(require scribblings/reference/mz scribble/decode scribblings/icons)

@title{Printing the Greek alphabet}

@codeblock{
#lang racket

;; Print the Greek alphabet
(for ([i 25])
  (displayln
   (integer->char
    (+ i (char->integer #\α)))))
}

@(define (guideintro* tag . s)
  (apply margin-note*
         (decode-content (append (list finger (guidesecref tag) " in " Guide " introduces ")
                                 s
                                 (list ".")))))

This program, like all the others we will see, start with @racketfont{#lang} on
the first line.  
@guideintro*["hash-lang"]{the @racketfont{#lang} notation}
This line specifies the @emph{language} that we will use for
the rest of the module.  In this case, the language is @racketmodname[racket],
which is the default language that we use for Racket programs.  The
@racketmodname[racket] language includes all of the core Racket language along
with lots of libraries.  The @racketmodname[racket/base] language is smaller,
including just the core language---using @racketmodname[racket/base] can be
useful to avoid the memory overhead of the larger @racketmodname[racket].
Other languages that you'll see often in Racket programs include 
@racketmodname[typed/racket], @racketmodname[racket/gui], 
@racketmodname[scribble/manual] and others.

The body of a module can have definitions of functions, as well as top-level
expressions which are run when the module is loaded.
The main body of this program is a @racket[for] loop. The @racket[for] form is
for iterating over some sequence of values, without producing any result. When
you need to produce a result, use a variant of @racket[for] such as 
@racket[for/list], @racket[for/hash], or @racket[for/vector], which
produce @tech[#:doc 'scribblings/guide/guide]{lists}, @tech{hashes}, 
and @tech{vectors} respectively.

The first part of a @racket[for] loop describes what it iterates over with 
a sequence of clauses.  Here, the only clause is @racket[[i 25]], which means
that @racket[i] will vary over all the integers from 0 to 24.  

The next line is a use of @racket[displayln].  This procedure takes anything as
input, and prints it out with a newline after it.

@margin-note*{By convention, conversion functions such as @racket[char->integer]
              use @racketidfont{->} in their names.}
Finally, we get to the real work---what we're going to print for each @racket[i].
We start with the integer value of the greek letter α, computing the
integer value using the @racket[char->integer] function. Finally, after adding
@racket[i] to get the @racket[i]th Greek letter, we convert back to a character
using @racket[integer->char] to get the result we want to display.

And that's it---a complete Racket program.



