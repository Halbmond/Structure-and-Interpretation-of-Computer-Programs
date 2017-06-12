#lang web-server/insta

;; A "hello world" web server
(define (start request)
  (response/xexpr
   '(html
     (body "Hello World"))))
