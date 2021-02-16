
#lang typed/racket

(require "../include/cs151-core.rkt")

(require typed/test-engine/racket-tests)

(define-type Nat 
  (U 'Z S)) 

(define-struct S 
  ([n : Nat]))

(: nat-equals? : Nat Nat -> Boolean)
(define (nat-equals? nat1 nat2)
  (cond (= nat1 nat2))
  (else #f))


;; (: nat-greater-than? : Nat Nat -> Boolean) 
;; (: nat-to-string : Nat -> String)