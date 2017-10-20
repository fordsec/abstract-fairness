; Implementation of the IMP language

#lang racket

(require math/distributions)
;https://docs.racket-lang.org/math/dist.html

(define (cmd? c) ;cmd is like a sequence as defined in FairSquare paper
  (match c
    [`skip #t] ;skip
    [`(set ,x ,(? expr?)) #t] ;set x as an expr (X <- exp)
    [`(prob-set ,x ,(? distribution?)) #t] ;prob-set x as a dist (X ~ dist)
    [`(begin ,(? cmd?) ...) #t] ;sequence of cmds
    [`(if ,(? expr?) then ,(? cmd?) else ,(? cmd?)) #t] ;conditional
    [else #f]))

(define (expr? e)
  ; "arithmetic expression over variables in V"
  ; V is set of real-valued variables that can appear in our program 
  (match e
    [(? number?) #t]
    [else #f]))

;(define (dist? e)
;  (match e
;    [`uniform #t]
;    ))

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
    