#lang racket

(define var? symbol?)

(define (expr? e)
  #t)

(define (stmt? s)
  (match s
    [`(,(? var?) <- ,(? expr?)) #t]
    [else #f]))

