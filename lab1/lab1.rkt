#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; Lab 1.1
;; I collaborated with Tejas. 

(: stop Image)
;; This is a picture of a stop sign :)
(define stop
(overlay (text "STOP" 70 "white")
           (star-polygon 90 8 1 "solid" "red")
           (star-polygon 95 8 1 "solid" "white")
           (star-polygon 100 8 1 "solid" "red")
           ))
stop

(: do-not-enter Image)
;; This is a Do Not Enter Sign in Drracket.
(define do-not-enter
  
    (overlay/align "center" "middle"
           (rectangle 150 25 "solid" "white")
           (circle 100 "solid" "red")
           (rectangle 200 200 "solid" "white")
           (rectangle 200 200 "outline" "black")
           ))
    (define DNE
           (above
            (text "DO NOT" 35 "white")
            (text " " 40 "red")
            (text "ENTER" 35 "white")))

(overlay/align "center" "middle" DNE do-not-enter)

(: yield Image)
;; My image of the Yield sign.
(define yield
(overlay/xy
   (text "YIELD" 25 "red")
   -63 -35
   (overlay/xy
    (triangle/sss 100 100 100 "solid" "white")
    -50 -30
            (overlay/xy
              (triangle/sss 190 190 190 "outline" "white")
              -4 -2
              (triangle/sss 200 200 200 "solid" "red")))))
       yield

(define yellow-diamond
  ;; yellow diamond function for just the yellow diamond and the black outline.
  (rotate 45 (overlay (rectangle 180 180 "solid" "gold")
           (rectangle 190 190 "solid" "black")
           (rectangle 200 200 "solid" "gold")
           )))

(define stoplight
  ;; stoplight image for the signal-ahead sign.
  (overlay 
            (above
                   (text " " 10 "black")
                   (circle 20 "solid" "firebrick")
                   (text " " 10 "black")
                   (circle 20 "solid" "gold")
                   (text " " 10 "black")
                   (circle 20 "solid" "forest green")
                   (text " " 10 "black")
                  )
            (rectangle 50 155 "solid" "black")))

(: signal-ahead Image)
;; My image of the "Signal Ahead" sign.
(define signal-ahead
  (overlay/align "center" "middle" stoplight yellow-diamond)
  )

signal-ahead

(define stop-no-text
  ;; this is an image of the stop sign without text.
(overlay
           (star-polygon 32 8 1 "solid" "red")
           (star-polygon 35 8 1 "solid" "white")
           (star-polygon 40 8 1 "solid" "red")
           ))

(define yield-no-text
  ;; yield sign without the text.
  (overlay
    (triangle/sss 60 60 60 "solid" "white")
    (triangle/sss 110 110 110 "outline" "white")
    (triangle/sss 120 120 120 "solid" "red")))

(define arrow
  ;; function for the arrow on top of the sign
  (above (triangle 50 "solid" "black")
         (rectangle 20 30 "solid" "black")
         ))

(: stop-ahead Image)
;; function for the stop ahead signal sign.
(define stop-ahead
  (overlay (above arrow (text "  " 30 "gold") stop-no-text
                   (text " " 15 "gold"))
           yellow-diamond))

stop-ahead

(: yield-ahead Image)
;; function for the yield ahead signal sign.
(define yield-ahead
  (overlay (above arrow (text "  " 30 "gold") yield-no-text
                   (text " " 15 "gold"))
           yellow-diamond))

yield-ahead
  
  
