; Implementation of the IMP language

#lang racket

(define (cmd? c)
  (match c
    [`skip #t]
    [`(set ,x ,(? expr?)) #t]
    [`(prob-set ,x ,(? dist?)) #t]
    [`(begin ,(? cmd?) ...) #t]
    [`(if ,(? expr?) then ,(? cmd?) else ,(? cmd?)) #t]
    [else #f]))

(define (expr? e)
  (match e
    [(? number?) #t]
    [(? symbol?) #t]
    [else #f]))

(define (dist? e)
  (match e
    [`uniform #t]))

(define example1
  '(if 0 then skip else (set x 1)))

(define example2
  '(begin (if 1 then (set x 0) else (set x 1)) (set y x)))

(define/contract (evaluate cmd environment)
  (cmd? hash? . -> . hash?)
  (display "calling evaluate...\n")
  (display cmd)
  (display "\n")
  (display environment)
  (display "\n")
  (match cmd
    [`skip environment]
    [`(set ,x ,e) (hash-set environment x (eval-expr e environment))]
    [`(begin ,cmds ...)
       (foldl (lambda (next-cmd cur-env) (evaluate next-cmd cur-env))
              environment
              cmds)]
    [`(if ,e then ,then-branch else ,else-branch)
     (if (not (= (eval-expr e environment) 0))
         (evaluate then-branch environment)
         (evaluate else-branch environment))]
    [else raise 'unexpected-cmd]))

(define (eval-expr e environment)
  (match e
    [(? symbol? x) (hash-ref environment x)]
    [(? number? n) n]))

; to run...
; (evaluate example2 (hash))
    