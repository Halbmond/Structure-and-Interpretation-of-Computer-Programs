#lang scribble/manual

@(require scribblings/reference/mz scribble/decode scribblings/icons)

@title{Unique lines}

@codeblock{
#lang racket

;; Report each unique line from stdin
(let ([saw (make-hash)])
  (for ([line (in-lines)])
    (unless (hash-ref saw line #f)
      (displayln line))
    (hash-set! saw line #t)))
}

This program keeps reading lines from standard input, and prints only the lines it hasn't seen before.

We start by creating a mutable hash table, named @racket[saw], with @racket[make-hash]. This table will let us keep track of every new line we see. 

Then, we use a @racket[for] loop to iterate over the lines read from @racket[stdin] with @racket[(in-lines)]
and bind each line to @racket[line] in the loop.

This loop will keep on going until @racket[eof] is read or until the program is forcibly terminated (because
@racket[(in-lines)] keeps reading until @racket[eof]). 

@margin-note*{Note that we can provide a default return value for @racket[hash-ref], in case the key is not found. 
Here, we use @racket[#f], the false value, so @racket[hash-ref] always returns a @tech{boolean}.}

To check if we've seen @racket[line] before, we look in our hash table with @racket[hash-ref]. 
Unless @racket[line] is in the table, we print it out with @racket[displayln] (@racket[displayln] prints its first argument followed by a newline, to @racket[stdout]).

Finally, because we need to remember every line we see, we store it in the @racket[saw] hash table 
with @racket[hash-set!].