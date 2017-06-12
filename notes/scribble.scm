#lang scribble/base[click for more]
@; Generate a PDF or HTML document
@title{Bottles: @italic{Abridged}}
@(apply
  itemlist
  (for/list ([n (in-range 100 0 -1)])
    @item{@(format "~a" n) bottles.}))
