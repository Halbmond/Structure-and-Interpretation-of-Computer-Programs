#lang datalog
ancestor(A, B) :- parent(A, B).
ancestor(A, B) :-
  parent(A, C), ancestor(C, B).
parent(john, douglas).
parent(bob, john).
ancestor(A, B)?
#;You can use Racket to build other languages — like datalog, a logic-programming language.
