#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(: first-name String)
(define first-name "Amatullah")

(: last-name String)
(define last-name "Mir")

(: platform String)
(define platform "Macintosh")

(: time-zone String)
(define time-zone "Central")

(: blue-star Image)
(define blue-star (star 50 "solid" "blue"))



