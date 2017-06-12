#lang scribble/manual

@(require scribblings/reference/mz scribble/decode scribblings/icons)

@title{An Echo Server}

@codeblock{
#lang racket

; An echo server
(define listener (tcp-listen 12345))

(let echo-server ()
  (define-values (in out) (tcp-accept listener))
  (thread (lambda () (copy-port in out)
                     (close-output-port out)))
  (echo-server))
}

This program again begins with @racketfont{#lang racket}.

We start by creating @racket[listener], using @racket[define].  This 
binds the name @racket[listener] in the rest of the module, which is
everything in the file.  @racket[listener] is the result of 
@racket[tcp-listen] on port @racket[12345], which produces a
@tech{TCP listener}.

We can then use @racket[listener] as the argument to @racket[tcp-accept]
to open a TCP connection.  The call to @racket[tcp-accept] inside of a
loop, written with the @racket[let] form.  When @racket[let] is followed
a name, 
@margin-note*{This is known as a @emph{named} @racket[let].}
such as @racket[echo-server] here, it creates a loop that can be 
continued by calling @racket[echo-server] again.  The loop we've written
here runs forever, because it always calls itself again.

In the body of the loop, we start by accepting a connection using
@racket[tcp-accept] on our @racket[listener].  Unlike other functions we've
seen before, this one produces @emph{two} values, so we bind these two values
with the @racket[define-values] to @racket[in] and @racket[out]. These two
values are both @tech{ports}, with @racket[in] an input port that gets the 
data sent by the other side of the TCP connection, and @racket[out] as an 
output port which sends data back to the other side.

Then we simply copy the data from @racket[in] to @racket[out] and close the
output port, terminating the connection.

However, we'd like to be able to accept multiple continuations at once, so
the copying of the ports is placed inside a separate @tech{thread}, meaning 
that we immediately return to the loop, and wait for another connection with
@racket[tcp-accept]. While this is happening, the thread that handles copying
the data does it work, and finishes when the copying is complete.