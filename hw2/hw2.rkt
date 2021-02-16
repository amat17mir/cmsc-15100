
#lang typed/racket

(require "../include/cs151-core.rkt")

(require typed/test-engine/racket-tests)

(define-type Nat 
  (U 'Z S)) 

(define-struct S 
  ([n : Nat]))

(define-type ArithTree 
  (U Leaf PlusTree TimesTree PowerTree))

(define-struct Leaf 
  ([n : Integer]))

(define-struct PlusTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-struct TimesTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-struct PowerTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-type StringTree 
  (U 'Empty StringNode))

(define-struct StringNode 
  ([root : String] 
   [lsub : StringTree] 
   [rsub : StringTree]))

(: nat-to-int : Nat -> Integer)
; converts a natural number into an integer.
(define (nat-to-int nat)
  (match nat
    ['Z 0]
    [(S n) (+ 1 (nat-to-int n))]))

(check-expect (nat-to-int 'Z) 0)
(check-expect (nat-to-int (S (S 'Z))) 2)
(check-expect (nat-to-int (S 'Z)) 1)

(: nat-equals? : Nat Nat -> Boolean)
; checks if one natural number is equal to the other natural number.
(define (nat-equals? nat1 nat2)
  (cond
    [(= (nat-to-int nat1) (nat-to-int nat2))]
    (else (error "not equal"))))

(check-expect (nat-equals? 'Z 'Z) #t)
(check-error (nat-equals? (S 'Z) 'Z) "not equal")
(check-expect (nat-equals? (S (S 'Z)) (S (S 'Z))) #t)

(: nat-greater-than? : Nat Nat -> Boolean)
; checks if one natural number is greater than the other natural number.
(define (nat-greater-than? nat1 nat2)
  (cond
    [(> (nat-to-int nat1) (nat-to-int nat2))]
    (else (error "not greater than"))))

(check-expect (nat-greater-than? (S 'Z) 'Z) #t)
(check-expect (nat-greater-than? (S (S (S 'Z))) (S (S ‘Z))) #t)
(check-error (nat-greater-than? 'Z 'Z) "not greater than")
(check-error (nat-greater-than? 'Z (S 'Z)) "not greater than")

(: nat-to-string : Nat -> String)
; converts the natural number into string form.
(define (nat-to-string nat)
  (match nat
    ['Z "Z"]
    [(S n) (string-append "S" (nat-to-string n))]))

(check-expect (nat-to-string 'Z) "Z")
(check-expect (nat-to-string (S 'Z)) "SZ")
(check-expect (nat-to-string (S (S 'Z))) "SSZ")
(check-expect (nat-to-string (S (S (S 'Z)))) "SSSZ")
(check-expect (nat-to-string (S (S (S (S 'Z))))) "SSSSZ")

(: eval : ArithTree -> Integer)
; evaluates the expression inputted in the tree.
(define (eval n)
  (match n
    [(Leaf n) n]
    [(PlusTree left right) (+ (eval left) (eval right))]
    [(TimesTree left right) (* (eval left) (eval right))]
    [(PowerTree left right) (expt (eval left) (abs (eval right)))]))


(: arith-tree-to-string : ArithTree -> String)
; converts an expression in the arithtree to a string
(define (arith-tree-to-string n)
  (match n
    [(Leaf n) (number->string n)]
    [(PlusTree right left)
     (string-append "(+ " (arith-tree-to-string right)"
                        " (arith-tree-to-string left) ")" )]
    [(TimesTree right left)
     (string-append "(* " (arith-tree-to-string right)"
                        " (arith-tree-to-string left) ")" )]
    [(PowerTree right left)
     (string-append "(^ " (arith-tree-to-string right)"
                        " (arith-tree-to-string left) ")" )]))

  
(: num-internal-nodes : ArithTree -> Integer)
; counts the number of internal nodes in a given tree.
(define (num-internal-nodes n)
  (match n
    [(Leaf n) 0]
    [(PlusTree right left)
     (+ 1 (num-internal-nodes right)(num-internal-nodes left))]
    [(TimesTree right left)
     (+ 1 (num-internal-nodes right)(num-internal-nodes left))]
    [(PowerTree right left)
     (+ 1 (num-internal-nodes right)(num-internal-nodes left))]))


(: num-leaves : ArithTree -> Integer)
; counts the leaves in the tree.
(define (num-leaves x)
  (match x
    [(Leaf n) 1]
    [(PlusTree right left) (+ (num-leaves right) (num-leaves left))]
    [(TimesTree right left) (+ (num-leaves right) (num-leaves left))]
    [(PowerTree right left) (+ (num-leaves right) (num-leaves left))]))


(: internal-nodes-to-leaves : ArithTree -> Exact-Rational)
; counts the ratio of internal leaves to nodes 
(define (internal-nodes-to-leaves n)
  (exact-round (/ (num-internal-nodes n)
     (num-leaves n))))


(: contains? : String StringTree -> Boolean)
; checks if a string is in the given tree 
(define (contains? str tre)
  (match tre
    ['Empty #f]
    [(StringNode root lsub rsub)
     (or (string=? root str)
         (contains? str rsub)
         (contains? str lsub))]))

(check-expect (contains? "S" tree1) #t)
(check-expect (contains? "Y" tree1) #t)


(: count : String StringTree -> Integer)
; counts the number of times a string appears in a tree.
(define (count str int)
  (match int
    ['Empty 0]
    [(StringNode root lsub rsub)
     (cond
       [(string=? root str) (+ 1 (count str lsub) (count str rsub))]
       [else (+ (count str lsub)) (count str rsub)])]))


(: total-length : StringTree -> Integer)
; calculates the entire length of strings within tree
(define (total-length n)
  (match n
    ['Empty 0]
    [(StringNode root lsub rsub) (+
                                  (string-length root)
                                  (total-length lsub)
                                  (total-length rsub))]))


(: same-tree? : StringTree StringTree -> Boolean)
; determines if the trees have the exact same shape and same strings
(define (same-tree? tree1 tree2)
  (match* (tree1 tree2)
    [('Empty 'Empty) #t]
    [((StringNode root1 lsub1 rsub1) (StringNode root2 lsub2 rsub2))
     (and (string=? root1 root2)
          (same-tree? lsub1 lsub2)
          (same-tree? rsub1 rsub2))]))


(: subset? : StringTree StringTree -> Boolean)
; determines if tree is contained in another tree.
(define (subset? tree1 tree2)
  (match tree1
    ['Empty #t]
    [(StringNode root lsub rsub)
     (and(contains? root tree2)
         (subset? lsub tree2)
         (subset? rsub tree1))]))


