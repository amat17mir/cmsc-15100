#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; list your collaborators in a comment: Kenny Jiang

(define-struct MenuItem
  ([kind  : (U 'appetizer 'entree 'dessert)]
   [name  : String]
   [price : Integer] ;; <-- integer num of pennies; floats aren't good for money
   [veg   : (U 'nonveg 'vegetarian 'vegan)]))

(define-type Menu
  (Listof MenuItem))

(: menu1 : Menu )
(define menu1
  (list (MenuItem 'appetizer "fries" 200 'vegetarian)
        (MenuItem 'entree "potatoes" 900 'vegetarian)
        (MenuItem 'appetizer "hashbrowns" 100 'vegetarian)
        (MenuItem 'dessert "potato pie" 500 'vegan)))

(: menu2 : Menu )
(define menu2
  (list (MenuItem 'appetizer "blueberries" 100 'vegan)
        (MenuItem 'entree "blueberry stew" 800 'vegetarian)
        (MenuItem 'dessert "blueberry pie" 600 'vegetarian)
        (MenuItem 'dessert "blueberry muffin" 300 'vegan)))

(: menu3 : Menu )
(define menu3
  (list (MenuItem 'appetizer "tomato soup" 200 'vegetarian)
        (MenuItem 'entree "tomato sandwich" 900 'vegetarian)
        (MenuItem 'entree "tomato stew" 500 'vegetarian)
        (MenuItem 'dessert "tomato juice" 100 'vegetarian)))

(: menu4 : Menu )
(define menu4
  (list (MenuItem 'entree "chicken" 800 'nonveg)
        (MenuItem 'entree "chicken sandwich" 800 'nonveg)
        (MenuItem 'entree "chicken pot pie" 600 'nonveg)
        (MenuItem 'dessert "icecream" 300 'vegetarian)))

(: menu5 : Menu )
(define menu5
  (list (MenuItem 'appetizer "tomato soup" 200 'vegetarian)))


(: num-appetizers : Menu -> Integer)
;; count number of appetizers a menu has and spits out the total.
(define (num-appetizers menu)
  (match menu
    ['() 0]
    [(cons (MenuItem 'appetizer _ _ _) jobr) (+ 1 (num-appetizers jobr))]
    [(cons (MenuItem 'entree _ _ _) jobr) (+ 0 (num-appetizers jobr))]
    [(cons (MenuItem 'dessert _ _ _) jobr) (+ 0 (num-appetizers jobr))]
    [(cons _ menu) (num-appetizers menu)]))

(check-expect (num-appetizers menu1) 2)
(check-expect (num-appetizers menu2) 1)
(check-expect (num-appetizers menu3) 1)
(check-expect (num-appetizers menu4) 0)
(check-expect (num-appetizers menu5) 1)


(: names-of-entrees : Menu -> (Listof String))
;; returns the name of the entrees on the menu.
(define (names-of-entrees entree)
  (match entree
    ['() '()]
    [(cons item items) (if (symbol=? (MenuItem-kind item) 'entree)
                           (cons (MenuItem-name item) (names-of-entrees items))
                           (names-of-entrees items))]))

(: vegetarian-friendly? : Menu -> Boolean)
;; determines whether or not there's a veggie option.
(define (vegetarian-friendly? veggies)
  (match veggies
    ['() #f]
    [(cons item items) (cond
                         [(or (symbol=? (MenuItem-veg item) 'vegan)
                              (symbol=? (MenuItem-veg item) 'vegetarian)) #t]
                         [else (vegetarian-friendly? items)])]))

(check-expect (vegetarian-friendly? menu1) #t)
(check-expect (vegetarian-friendly? menu2) #t)
(check-expect (vegetarian-friendly? menu3) #t)
(check-expect (vegetarian-friendly? menu4) #t)

(: num-vegetarian-items : Menu -> Integer)
;; counts the number of vegetarian items.
(define (num-vegetarian-items menu)
  (match menu
    ['() 0]
    [(cons (MenuItem _ _ _ 'vegetarian) n) (+ 1 (num-vegetarian-items n))]
    [(cons (MenuItem _ _ _ 'vegan) n) (+ 1 (num-vegetarian-items n))]
    [(cons (MenuItem _ _ _ 'nonveg) n) (+ 0 (num-vegetarian-items n))]))

(check-expect (num-vegetarian-items menu1) 4)
(check-expect (num-vegetarian-items menu2) 4)
(check-expect (num-vegetarian-items menu3) 4)

(: num-vegan-items : Menu -> Integer)
;; counts the number of vegetarian items.
(define (num-vegan-items menu)
  (match menu
    ['() 0]
    [(cons (MenuItem _ _ _ 'vegetarian) n) (+ 0 (num-vegan-items n))]
    [(cons (MenuItem _ _ _ 'vegan) n) (+ 1 (num-vegan-items n))]
    [(cons (MenuItem _ _ _ 'nonveg) n) (+ 0 (num-vegan-items n))]))

(check-expect (num-vegan-items menu1) 1)
(check-expect (num-vegan-items menu2) 2)
(check-expect (num-vegan-items menu3) 0)
(check-expect (num-vegan-items menu4) 0)
(check-expect (num-vegan-items menu5) 0)

(: all-items : Menu -> Integer)
;; counts the number of vegetarian items.
(define (all-items menu)
  (match menu
    ['() 0]
    [(cons (MenuItem _ _ _ 'vegetarian) n) (+ 1 (all-items n))]
    [(cons (MenuItem _ _ _ 'vegan) n) (+ 1 (all-items n))]
    [(cons (MenuItem _ _ _ 'nonveg) n) (+ 1 (all-items n))]))

(check-expect (all-items menu1) 4)
(check-expect (all-items menu2) 4)
(check-expect (all-items menu3) 4)
(check-expect (all-items menu4) 4)
(check-expect (all-items menu5) 1)

(: vegan-items : Menu -> Menu)
;; returns a new menu with just vegan options.
(define (vegan-items vegan)
  (match vegan
      ['() '()]
      [(cons item items) (if (symbol=? (MenuItem-veg item) 'vegan)
                         (cons item (vegan-items items))
                         (vegan-items items))]))

(check-expect (vegan-items menu1) (list (MenuItem 'dessert "potato pie" 500 'vegan)))
(check-expect (vegan-items menu2) (list (MenuItem 'appetizer "blueberries" 100 'vegan)
                                        (MenuItem 'dessert "blueberry muffin" 300 'vegan)))
(check-expect (vegan-items menu3) '())


(: vegan-ratio : Menu -> Exact-Rational)
;; returns the ratio of vegan options to non-vegan options.
(define (vegan-ratio ratio)
  (/ (num-vegan-items ratio) (all-items ratio)))

(check-expect (vegan-ratio menu1) 1/4)
(check-expect (vegan-ratio menu2) 1/2)
(check-expect (vegan-ratio menu3) 0)
(check-expect (vegan-ratio menu4) 0)

(: total-desserts : Menu -> Integer )
;; total number of desserts in a menu
(define (total-desserts desserts)
  (match desserts
    ['() 0]
    [(cons item items)(cond
                        [(symbol=? (MenuItem-kind item) 'dessert)
                         (+ 1 (total-desserts items))]
                        [else (total-desserts items)])]))

(check-expect (total-desserts menu1) 1)
(check-expect (total-desserts menu2) 2)

(: price-desserts : Menu -> Integer)
;; calculate the price of the desserts in the menu.
(define (price-desserts price)
  (match price
    ['() 0]
    [(cons item items) (cond [(symbol=? (MenuItem-kind item) 'dessert)
                              (+ (MenuItem-price item) (price-desserts items))]
                             [else (price-desserts items)])]))

(check-expect (price-desserts menu1) 500)
(check-expect (price-desserts menu2) 900)
(check-expect (price-desserts menu3) 100)

                          
(: average-dessert-price : Menu -> Exact-Rational)
;; takes in a list of menus and computes the average price of desserts.
(define (average-dessert-price menu)
  (match menu
    ['() 0]
    [(cons item items) (/ (price-desserts menu) (total-desserts menu))]))

(check-expect (average-dessert-price menu1) 500)
(check-expect (average-dessert-price menu2) 450)
(check-expect (average-dessert-price menu3) 100)

(: menus-with-cheaper-desserts : (Listof Menu) -> (Listof Menu))
;; takes in a list of menus, computes the average price of desserts,
;; and returns the ones that cost $5 or less.
(define (menus-with-cheaper-desserts menus)
  (match menus
    ['() '()]
    [(cons item items) (if (< (average-dessert-price item) 500)
                           (cons item (menus-with-cheaper-desserts items))
                           (menus-with-cheaper-desserts items))]))

(: most-expensive : Menu -> MenuItem)
;; returns the most expensive item on the menu.
(define (most-expensive items)
  (match items
    ['() (error "most-expensive: no jobs")]
    [(cons item '()) item]
    [(cons item itemr)
     (local
       {(: most-expensive-all MenuItem)
        (define most-expensive-all (most-expensive itemr))}
       (match* (item most-expensive-all)
         [((MenuItem _ _ price1 _) (MenuItem _ _ price2 _))
          (if (< (/ price1) (/ price2))
              item
              most-expensive-all)]))]))

(check-expect (most-expensive menu1) (MenuItem 'entree "potatoes" 900 'vegetarian)) 
(check-expect (most-expensive menu2) (MenuItem 'entree "blueberry stew" 800 'vegetarian)) 
(check-expect (most-expensive menu3) (MenuItem 'entree "tomato sandwich" 900 'vegetarian)) 


(test)