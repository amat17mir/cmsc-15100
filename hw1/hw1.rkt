#lang typed/racket

(require "../include/cs151-core.rkt")

(require typed/test-engine/racket-tests)

(define-struct TaxReturn
  ([income : Integer]
   [charity : Integer]
   [num-adults : Integer]
   [num-children : Integer]
   [purchased-EV? : Boolean]
   [covid-costs : Integer]
   [already-paid : Integer]))

(: plausible? : TaxReturn -> Boolean)
;; Checks if the TaxReturn is plausible if the number
;; of adults is positive, and if the income, charity, 
;; number of children, covid-costs, and already-paid amounts
;; are all non-negative.
(define (plausible? n)
      (and [> (TaxReturn-num-adults n) 0]
           [>= (TaxReturn-income n) 0] 
           [>= (TaxReturn-num-children n) 0]
           [>= (TaxReturn-covid-costs n) 0]
           [>= (TaxReturn-already-paid n) 0]))

(check-expect (plausible? (TaxReturn 2400 0 1 6 #t 0 0)) #t)
(check-expect (plausible? (TaxReturn 2400 0 -1 6 #t 0 0)) #f)

(: adjusted-income : TaxReturn -> Integer)
;; Subtracts the income from the sum of the charity and covid-
;; costs. Must be nonnegative.
(define (adjusted-income tax)
  (cond
    [>= tax 0 (- (TaxReturn-income tax)
                 (+ (TaxReturn-charity tax) (TaxReturn-covid-costs tax)))]
    [else 0]))

(check-expect (adjusted-income (TaxReturn 200 20 1 1 #t 20 0)) 160)
(check-expect (adjusted-income (TaxReturn 400 20 1 1 #t 20 0)) 360)

(: income-tax : TaxReturn -> Integer)
;; Incorporates income tax according to adjusted income.
(define (income-tax amt)
  (cond
     [(<= (adjusted-income amt) 10000)
     (exact-ceiling (* 0 (adjusted-income amt)))]
     
    [(>= (adjusted-income amt) 10001)
     (exact-ceiling (* 0.2
              (- (adjusted-income amt) 10000)))]
    
    [(>= (adjusted-income amt) 20001)
     (exact-ceiling (+ (* 0.3
              (- (adjusted-income amt) 20000)) (* 0.2 10000)))]
    
    [(>= (adjusted-income amt) 40001)
     (exact-ceiling (+ (+ (* 0.4
              (- (adjusted-income amt) 40000)) (* 0.3 20000)) (* 0.2 10000)))]
    
     [(>= (adjusted-income amt) 80001)
     (exact-ceiling (+ (+ (+ (* 0.5
              (- (adjusted-income amt) 80000)) (* 0.4 40000)) (* 0.3 20000))
                       (* 0.2 10000)))]
     [else 0]))

(check-expect (income-tax (TaxReturn 900 20 1 1 #t 20 0)) 0)
(check-expect (income-tax (TaxReturn 12000 0 1 0 #t 0 0)) 400)
(check-expect (income-tax (TaxReturn 21000 0 1 0 #t 0 0)) 2200)


(: child-credit : TaxReturn -> Integer)
;; Multiplies 1200 for each child in the family.
(define (child-credit n)
  (cond
    [(>= (TaxReturn-num-children n) 0 ) (* (TaxReturn-num-children n) 1200)]
  [else 0]))

(check-expect (child-credit (TaxReturn 2400 0 1 6 #t 0 0)) 7200)
(check-expect (child-credit (TaxReturn 2400 0 1 2 #t 0 0)) 2400)

(: ev-credit : TaxReturn -> Integer)
;; Multiplies the number of people in the family by 500 if
;; a car has been purchased. If not, then EV credit = 0.
(define (ev-credit car)
  (if (TaxReturn-purchased-EV? car) #f 0)
      (* (+ (TaxReturn-num-adults car) (TaxReturn-num-children car)) 500))

(check-expect (ev-credit (TaxReturn 2400 0 1 6 #t 0 0)) 3500)
(check-expect (ev-credit (TaxReturn 2400 0 3 10 #t 0 0)) 6500)


(: balance : TaxReturn -> Integer)
;; Income tax minus the child credit minus the ev-credit.
(define (balance x)
 (- (income-tax x) (+ (child-credit x)(ev-credit x))))

(check-expect (balance (TaxReturn 2400 0 1 6 #t 0 0)) -10700)
(check-expect (balance (TaxReturn 7800 0 3 1 #t 0 0)) -3200)


(: refund? : TaxReturn -> Boolean)
;; If the balance is negative, then the refund is true. If not,
;; then the refund is false.
(define (refund? money)
  (cond
    [(< (balance money) 0) #t]
  [else #f]))

(check-expect (refund? (TaxReturn 2400 0 1 6 #t 0 0)) #t)

(test)
