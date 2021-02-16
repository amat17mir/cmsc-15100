#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
  
(require typed/test-engine/racket-tests)

;; === data definitions

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct Click
  ([x : Integer]
   [y : Integer]))

(define-struct CircleWorld
  ([background-color : Image-Color]
   [initial-color    : Image-Color]
   [change-to-color  : Image-Color]
   [num-circles : Integer]
   [radius  : Integer]
   [padding : Integer]
   [clicked-on? : (Listof Boolean)]))

;; == sample world, useful for testing

(define world0
  (CircleWorld 'lightblue
               'gray
               'dodgerblue
               12
               20
               8
               (make-list 12 #f)))

;; === operations and calculations

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
;; ex: (replace-at 0 'Z '(a b c)) -> '(Z b c)
;; ex: (replace-at 1 'Z '(a b c)) -> '(a Z c)
(define (replace-at i x xs)
  (error "todo: replace-at"))

(: clicked-within : Click CircleWorld -> (Optional Integer))
;; assume each circle has an index from 0 to (n-1), counting from the left
;; if the click is within circle i, this function returns (Some i)
;; otherwise 'None
(define (clicked-within click world)
  (error "todo: clicked-within"))

;; === universe support

(: draw : CircleWorld -> Image)
;; draw the CircleWorld, taking care that padding is correct
(define (draw world)
  (error "todo: draw-world"))

(: react-to-mouse : CircleWorld Integer Integer Mouse-Event -> CircleWorld)
;; if the user clicks on a circle, change the color to the "change-to-color"
;; if the user clicks outside any circle, reset all circles to the initial color
(define (react-to-mouse world x y e)
  (error "todo: react-to-mouse"))

(: run : Image-Color Image-Color Image-Color Integer Integer Integer 
   -> CircleWorld)
;; run the world given setup details
(define (run bg init ch n r p)
  (error "todo: run"))

(test)
