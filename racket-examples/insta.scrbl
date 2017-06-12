#lang scribble/manual

@(require scribblings/reference/mz scribble/decode scribblings/icons)

@title{A Simple Web Server}

@codeblock{
#lang web-server/insta

;; A "hello world" web server
(define (start request)
  (response/xexpr
   '(html
     (body "Hello World"))))}

This program uses a new language that we haven't seen 
before: @racketmodname[web-server/insta].  This language
is designed for writing simple web servers. The only requirement
is that the module defines a function with the name @racket[start], 
which consumes an HTTP request and produces a @tech{response}.

The @racket[start] function in this module always responds with the same
web page, which just says @bold{Hello World}.  We generate an HTML page 
using the XExpr syntax, which uses s-expressions to represent HTML. This
is turned into an HTTP response with the @racket[response/xexpr] procedure, 
@margin-note*{The @racket[response/full] lets you specify all aspects of the response.}
which is the response that our server sends to every client.


