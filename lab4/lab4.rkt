
#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; list your collaborators in a comment: Matthew Akuwaza, Jack Ogle

(define-struct MenuItem
  ([kind  : (U 'appetizer 'entree 'dessert)]
   [name  : String]
   [price : Integer] ;; <-- integer num of pennies; floats aren't good for money
   [veg   : (U 'nonveg 'vegetarian 'vegan)]))

;; *inexact numbers aren't good for representing money

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
  (list (MenuItem 'appetizer "tomato soup" 200 'vegan)
        (MenuItem 'entree "tomato sandwich" 900 'vegan)
        (MenuItem 'entree "tomato stew" 500 'vegan)
        (MenuItem 'dessert "tomato juice" 100 'vegan)))

(: menu4 : Menu )
(define menu4
  (list (MenuItem 'entree "chicken" 800 'nonveg)
        (MenuItem 'entree "chicken sandwich" 800 'nonveg)
        (MenuItem 'entree "chicken pot pie" 600 'nonveg)
        (MenuItem 'dessert "icecream" 300 'nonveg)))

(: menu5 : Menu )
(define menu5
  (list (MenuItem 'appetizer "tomato soup" 200 'vegetarian)))


(: vegan-items : Menu -> Menu)
;; inputs a menu and returns another menu with JUST vegan options.
;; uses filter function.
(define (vegan-items menu)
  (filter (lambda ([n : MenuItem]) (symbol=? 'vegan (MenuItem-veg n))) menu))

;; check expect functions:
(check-expect (vegan-items menu1)
              (list (MenuItem 'dessert "potato pie" 500 'vegan)))
(check-expect (vegan-items menu2)
              (list (MenuItem 'appetizer "blueberries" 100 'vegan)
                    (MenuItem 'dessert "blueberry muffin" 300 'vegan)))
(check-expect (vegan-items menu3)
              (list (MenuItem 'appetizer "tomato soup" 200 'vegan)
                    (MenuItem 'entree "tomato sandwich" 900 'vegan)
                    (MenuItem 'entree "tomato stew" 500 'vegan)
                    (MenuItem 'dessert "tomato juice" 100 'vegan)))
(check-expect (vegan-items menu4) '())

(: prices : Menu -> (Listof Integer))
;; returns a list of the prices of the menu items.
;; uses the map function.
(define (prices menu)
    (map (lambda ([i : MenuItem]) (MenuItem-price i)) menu))

(check-expect (prices menu1)
              (list 200 900 100 500)) 
(check-expect (prices menu2)
              (list 100 800 600 300))
(check-expect (prices menu3)
              (list 200 900 500 100))
(check-expect (prices menu4)
              (list 800 800 600 300))

(: biggest-menu-item : MenuItem MenuItem -> MenuItem)
;; useful function to call in 'most-expensive' function.
(define (biggest-menu-item item1 item2)
  (if (> (MenuItem-price item1) (MenuItem-price item2))
      item1
      item2))

(: most-expensive : Menu -> MenuItem)
;; returns the most expensive item from the list.
(define (most-expensive menu)
  (foldr biggest-menu-item (MenuItem 'appetizer "" 0 'vegan) menu))

(check-expect (most-expensive menu1)
              (MenuItem 'entree "potatoes" 900 'vegetarian))
(check-expect (most-expensive menu2)
              (MenuItem 'entree "blueberry stew" 800 'vegetarian))

(: vegetarian-friendly? : Menu -> Boolean)
;; determines whether or not an item has a vegetarian item. if it does,
;; it returns true.
(define (vegetarian-friendly? menu)
  (ormap (lambda ([n : MenuItem]) (symbol=? 'vegan (MenuItem-veg n))) menu))

(check-expect (vegetarian-friendly? menu1) #t)
(check-expect (vegetarian-friendly? menu2) #t)
(check-expect (vegetarian-friendly? menu3) #t)
(check-expect (vegetarian-friendly? menu4) #f)

(: names-of-entrees : Menu -> (Listof String))
;; takes in a menu and returns a list of the names of the entrees.
(define (names-of-entrees menu)
  (map (lambda ([i : MenuItem]) (MenuItem-name i))
        (filter (lambda ([n : MenuItem])
                  (symbol=? 'entree (MenuItem-kind n))) menu)))

(check-expect (names-of-entrees menu1) (list "potatoes"))
(check-expect (names-of-entrees menu2) (list "blueberry stew"))
(check-expect (names-of-entrees menu3)
              (list "tomato sandwich" "tomato stew"))
(check-expect (names-of-entrees menu4)
              (list "chicken" "chicken sandwich" "chicken pot pie"))

(: count-if : All (A) (A -> Boolean) (Listof A) -> Integer)
;; counting function for specified conditions.
(define (count-if test int)
  (match int
    ['() 0]
    [(cons head tail) (if (test head)
                          (+ 1 (count-if test tail))
                          (count-if test tail))]))

(: num-appetizers : Menu -> Integer)
;; counts the number of appetizers in a menu.
(define (num-appetizers menu)
  (count-if (lambda ([n : MenuItem])
              (symbol=? 'appetizer (MenuItem-kind n))) menu))

(check-expect (num-appetizers menu1) 2)
(check-expect (num-appetizers menu2) 1)
(check-expect (num-appetizers menu3) 1)
(check-expect (num-appetizers menu4) 0)

(: vegan-ratio : Menu -> Exact-Rational)
;; returns the ratio of vegan options to non vegan options.
(define (vegan-ratio menu)
  (/(count-if (lambda ([n : MenuItem])
                 (symbol=? (MenuItem-veg n) 'vegan)) menu)
               (length menu)))

(check-expect (vegan-ratio menu1) 1/4)
(check-expect (vegan-ratio menu2) 1/2)
(check-expect (vegan-ratio menu3) 1)
(check-expect (vegan-ratio menu4) 0)
                          
(: average-dessert-price : Menu -> Exact-Rational)
;; takes in a list of menus and computes the average price of desserts.
(define (average-dessert-price menu)
  (local
    {(: dessert? : MenuItem -> Boolean)
     (define (dessert? n) (symbol=? (MenuItem-kind n) 'dessert))
     (: num : MenuItem -> Integer)
     (define (num n) (MenuItem-price n))
     (define desserts (map num (filter dessert? menu)))}
    (/ (foldr + 0 desserts) (length desserts))))

(check-expect (average-dessert-price menu1) 500)
(check-expect (average-dessert-price menu2) 450)
(check-expect (average-dessert-price menu3) 100)


(: num-menus-with-cheaper-desserts : (Listof Menu) Integer -> Integer)
;; returns a number of the menus with desserts under the threshold.
(define (num-menus-with-cheaper-desserts menu cap)
  (local
   {(: check-price? : Menu -> Boolean)
    (define (check-price? menu)
      (>= cap (average-dessert-price menu)))}
   (count-if check-price? menu)))

                     
(test)

