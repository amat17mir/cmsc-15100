#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

(define-struct TriangleWorld
  ([color : (U 'green 'pink'orange)]
   [side : Integer]
   [theta : Integer]
   [message : String]))

(: draw : TriangleWorld -> Image)
;; draw triangle of a given size in a certain color.
(define (draw world)
  (match world
    [(TriangleWorld color side theta msg)
     (overlay (text msg 20 'black)
              (rotate theta (triangle side 'solid color)))]))

(draw (TriangleWorld 'green 100 0 ""))
(draw (TriangleWorld 'pink 200 60 ""))
(draw (TriangleWorld 'orange 100 0 "Amatullah"))
(draw (TriangleWorld 'pink 200 60 "Pink"))

(: react : TriangleWorld String -> TriangleWorld)
;; turn a triangle orange with a lowercase "o"
(define (react world k)
  (match world
    [(TriangleWorld color size theta msg)
     (match k
       ["o" (TriangleWorld 'orange size theta msg)]
       ["g" (TriangleWorld 'green size theta msg)]
       ["p" (TriangleWorld 'pink size theta msg)]
       ["-" (TriangleWorld color (- size 5) theta msg)]
       ["+" (TriangleWorld color (- size 5) theta msg)]
       ["r" (TriangleWorld color size (+ 10 theta) msg)]
       ["a" (TriangleWorld color size theta (string-append msg "a"))]
       ["b" (TriangleWorld color size theta (string-append msg "b"))]
       ["escape" (TriangleWorld color size theta "")]
       [_ world])]))

(: create : (U 'green 'pink 'orange) Integer -> TriangleWorld)
;; open a new world
(define (create initial-color initial-side)
  (big-bang (TriangleWorld initial-color initial-side 0 "") : TriangleWorld
    [to-draw draw]
    [on-key react]))

(test)
