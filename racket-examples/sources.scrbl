#lang scribble/manual

@(require scribblings/reference/mz scribble/decode scribblings/icons)

@title{Finding Racket sources}

@codeblock{
#lang racket
;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))
}

This program finds all the files with the suffix @tt{.rkt}.

Again we use a @racket[for] loop, but here we're using the 
@racket[(in-directory)] function to iterate over
everything in a directory. With no arguments, as here,
@racket[in-directory] searches the current directory.
@margin-note*{In fact, @racket[in-directory] uses the value of
              the @racket[current-directory], which can be changed.}
The path of each file in the directory is bound to @racket[path] in 
the loop.

Inside the loop, we check if the path matches the regular expression
@racket[#rx"[.]rkt$"]. Literal regular expressions start with @racketfont{#rx}.
This regular expression describes anything 
that starts with @racketfont{.} (the @racketfont{[]} are required 
because @racketfont{.} is usually treated specially by regular expressions).
Then come the characters @racketfont{rkt}, followed by @racketfont{$}, which
indicates the end of the string. 

We check that our regular expression matches @racket[path] using
@racket[regexp-matches?], which works on strings, paths,
and many other inputs.
If the regular expression matches @racket[path], we run the rest of our code.  
This is indicated by using @racket[when], which checks its first expression, 
and if that's true, it runs the rest of the expressions.

When @racket[path] is the path to a Racket source file, then we print
the name of the source file using the @racket[printf] procedure. The first argument
to @racket[printf] is a @tech{format string}, which specifies what is
printed.  The @racketfont{~a} in the format string is replaced with @racket[path].


