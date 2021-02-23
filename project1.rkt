#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

(define-struct CalFormat
  ([cell-size : Integer]
   [title-bar-bg : Image-Color]
   [title-bar-font : Image-Color]
   [title-bar-height : Integer]
   [day-label-bg : Image-Color]
   [day-label-font : Image-Color]
   [day-label-height : Integer]
   [cell-bg : Image-Color]
   [cell-font : Image-Color]))

(define-struct CalWorld
  ([format : CalFormat]
   [current-month : Integer]
   [current-year : Integer]))

(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

(define-type Day
  (U 'Su 'M 'Tu 'W 'Th 'F 'Sa))

;; note: for the month, 1 means January, 2 means February, etop.

(define-struct Day-Of-Week
  ([num : Integer]))

;; note: 0 means Sunday, 1 means Monday, ..., 6 means Saturday

(: leap? (-> Integer Boolean))
;; determines whether or not the year inputted is a leap year.
(define (leap? year)
  (or (and (= (modulo year 4) 0)

           (not (= (modulo year 100) 0)))
      (= (modulo year 400) 0)))

(check-expect (leap? 2020) #t)
(check-expect (leap? 1991) #f)
(check-expect (leap? 2056) #t)

(: days-in-month (-> Integer Integer Integer))
;; input is the month and year, output is the number of days. program
;; considers the number of days depending on the month, and the year
;; being a leap year or not.
(define (days-in-month m y)
  (cond
    [(and (= m 2)
          (leap? y)) 29]
    [(and (= m 2)
          (leap? y)) 28]
    [(= m 1) 31]
    [(= m 3) 31]
    [(= m 4) 30]
    [(= m 5) 31]
    [(= m 6) 30]
    [(= m 7) 31]
    [(= m 8) 31]
    [(= m 9) 30]
    [(= m 10) 31]
    [(= m 11) 30]
    [(= m 12) 31]
    [else (error "month is not in range 1-12")]))

(check-expect (days-in-month 2 2020) 29)
(check-expect (days-in-month 5 2001) 31)
(check-expect (days-in-month 6 1984) 30)


(: smart-date (-> Integer Integer Integer Date))
; determines whether or not the date inputted is valid.
(define (smart-date m d y)
  (cond
    [(or (< m 1) (> m 12)) (error "month out of range")]
    [(or (< d 1) (> d (days-in-month m y))) (error "day out of range")]
    [(< y 1900) (error "year out of range")]
    [else (Date m d y)]))

(check-expect (smart-date 1 27 2020) (Date 1 27 2020))
(check-error (smart-date 21 27 2020) "month out of range")


(: date=? (-> Date Date Boolean))
;; determines if two dates are equal.
(define (date=? a1 a2)
  (match* (a1 a2)
    [((Date m1 d1 y1)
      (Date m2 d2 y2))
     (and (= m1 m2)
          (= d1 d2)
          (= y1 y2))]))

(check-expect (date=? (Date 1 27 2021) (Date 1 27 2021)) #t)
(check-expect (date=? (Date 5 9 2001) (Date 5 9 2001)) #t)
(check-expect (date=? (Date 3 22 1997) (Date 3 29 1968)) #f)


(: date<? (-> Date Date Boolean))
;; determines if the first date comes before the second.
(define (date<? a3 a4)
  [match* (a3 a4)
    [((Date m1 d1 y1) (Date m2 d2 y2))
     (cond
       [(< y1 y2)]
       [(= y1 y2) (< m1 m2)]
       [(= y1 y2) (< m1 m2) (< d1 d2)]
       [(= y1 y2) (= m1 m2) (< d1 d2)]
       [(= y1 y2) (= m1 m2) (> d1 d2)]
       [(= y1 y2) (> m1 m2) (= d1 d2)]
       [(= y1 y2) (= m1 m2) (= d1 d2)]
       [else (error "dates are equal")])]])


(check-expect (date<? (Date 1 27 2021) (Date 2 27 2021)) #t)
(check-expect (date<? (Date 1 28 2021) (Date 1 27 2021)) #f)
(check-expect (date<? (Date 1 27 2021) (Date 1 27 2022)) #t)
(check-expect (date<? (Date 1 27 2021) (Date 1 27 2021)) #f)

      
(: days-after (-> Day-Of-Week Integer Day-Of-Week))
;; determines the number of days after the date inputted.
(define (days-after day d)
  (cond
    [(and (>= (Day-Of-Week-num day) 0) (<= (Day-Of-Week-num day) 6))
     (Day-Of-Week (modulo (+ (Day-Of-Week-num day) d) 7))]
    [else (error "day falls out of range")]))

(check-expect (days-after (Day-Of-Week 3) 6) (Day-Of-Week 2))
(check-expect (days-after (Day-Of-Week 5) 13) (Day-Of-Week 4))
(check-error (days-after (Day-Of-Week 16) 13) "day falls out of range")
(check-error (days-after (Day-Of-Week -16) 13) "day falls out of range")
              
(: doomsday-in-month (-> Integer Integer Integer))
;; outputs the doomsday in the month.
(define (doomsday-in-month m y)
  (cond
    [(or (= m 4) (= m 6) (= m 8) (= m 10) (= m 12)) m]
    [(and (= m 1) (leap? y)) 32]
    [(and (= m 2) (leap? y)) 29]
    [(and (= m 1) (not (leap? y))) 31]
    [(and (= m 2) (not (leap? y))) 28]
    [(= m 3) 0]
    [(= m 5) 9]
    [(= m 7) 11]
    [(= m 9) 5]  
    [(= m 11) 7]
    [else (error "not in range")]))

(check-expect (doomsday-in-month 2 2020) 29)
(check-expect (doomsday-in-month 1 2020) 32)
(check-expect (doomsday-in-month 2 2019) 28)
(check-expect (doomsday-in-month 1 2019) 31)
(check-expect (doomsday-in-month 3 2020) 0)
(check-expect (doomsday-in-month 4 2020) 4)
(check-expect (doomsday-in-month 5 2010) 9)
(check-expect (doomsday-in-month 6 2020) 6)
(check-expect (doomsday-in-month 7 2023) 11)
(check-expect (doomsday-in-month 8 2020) 8)
(check-expect (doomsday-in-month 9 1990) 5)
(check-expect (doomsday-in-month 10 2020) 10)
(check-expect (doomsday-in-month 11 2020) 7)
(check-expect (doomsday-in-month 12 2020) 12)
(check-error (doomsday-in-month 13 2020) "not in range")

(: doomsday-in-century (-> Integer Day-Of-Week))
;; calculates the doomsday of the century
(define (doomsday-in-century y)
  (match (modulo (floor (/ y 100)) 4)
    [0 (Day-Of-Week 2)]
    [1 (Day-Of-Week 0)]
    [2 (Day-Of-Week 5)]
    [3 (Day-Of-Week 3)]))

(check-expect (doomsday-in-century 2100) (Day-Of-Week 0))
(check-expect (doomsday-in-century 1900) (Day-Of-Week 3))
(check-expect (doomsday-in-century 2000) (Day-Of-Week 2))
(check-expect (doomsday-in-century 2200) (Day-Of-Week 5))
(check-expect (doomsday-in-century 2104) (Day-Of-Week 0))
(check-expect (doomsday-in-century 1590) (Day-Of-Week 3))

(: doomsday-in-year (-> Integer Day-Of-Week))
;; calculates the doomsday of the year.

(define (doomsday-in-year y)
  (Day-Of-Week (modulo (+ (Day-Of-Week-num (doomsday-in-century y))
                          (modulo y 100)
                          (floor (/ (modulo y 100) 4))) 7)))

(check-expect (doomsday-in-year 2020) (Day-Of-Week 6))
(check-expect (doomsday-in-year 2013) (Day-Of-Week 4))
(check-expect (doomsday-in-year 1678) (Day-Of-Week 1))


(: find-day-of-week (-> Date Day-Of-Week))
; calculates the day of the week using the date inputted.
(define (find-day-of-week d)
  (Day-Of-Week (modulo
                (cond
                  [(> (doomsday-in-month (Date-m d) (Date-y d)) (Date-d d))
                   (- (Day-Of-Week-num (doomsday-in-year (Date-y d)))
                      (- (doomsday-in-month (Date-m d) (Date-y d))
                         (Date-d d)))]
                  [else 
                   (+ (Day-Of-Week-num (doomsday-in-year (Date-y d)))
                      (- (Date-d d)
                         (doomsday-in-month (Date-m d) (Date-y d))))]) 7)))

(check-expect (find-day-of-week (Date 1 28 2021)) (Day-Of-Week 4))
(check-expect (find-day-of-week (Date 1 15 1998)) (Day-Of-Week 4))
(check-expect (find-day-of-week (Date 2 14 1775)) (Day-Of-Week 2))
(check-expect (find-day-of-week (Date 5 7 2100)) (Day-Of-Week 5))
(check-expect (find-day-of-week (Date 7 15 1998)) (Day-Of-Week 3))

(: day-to-month : Date -> Integer)
;; converts a number to its corresponding month
(define (day-to-month date)
  (match date
    [(Date m d y) (cond
                    [(= m 1) 31]
                    [(and (= m 2)
                          (leap? y)) 29]
                    [(= m 2) 28]
                    [(= m 3) 31]
                    [(= m 4) 30]
                    [(= m 5) 31]
                    [(= m 6) 30]
                    [(= m 7) 31]
                    [(= m 8) 31]
                    [(= m 9) 30]
                    [(= m 10) 31]
                    [(= m 11) 30]
                    [(= m 12) 31]
                    [else (error "Month out of range")])]))

(: day-to-integer : Day -> Integer)
;; converts a day to its number
(define (day-to-integer d)
  (cond
    [(symbol=? d 'M) 1]
    [(symbol=? d 'Tu) 2]
    [(symbol=? d 'W) 3]
    [(symbol=? d 'Th) 4]
    [(symbol=? d 'F) 5]
    [(symbol=? d 'Sa) 6]
    [else 0]))

(: integer-to-month : Integer -> String)
;; converts a number to its corresponding month
(define (integer-to-month m)
  (cond
    [(= m 1) "Jan"]
    [(= m 2) "Feb"]
    [(= m 3) "March"]
    [(= m 4) "April"]
    [(= m 5) "May"]
    [(= m 6) "June"]
    [(= m 7) "July"]
    [(= m 8) "Aug"]
    [(= m 9) "Sept"]
    [(= m 10) "Oct"]
    [(= m 11) "Nov"]
    [(= m 12) "Dec"]
    [else (error "Month out of range")]))


(: banner : Integer Integer Integer Image-Color Image-Color Integer -> Image)
;; produces the title with specified background and text
;; color of a given month and year
(define (banner mon yr img col1 col2 h)
  (overlay (text (string-append (integer-to-month mon) " "
                                (number->string yr)) (cast img Byte) col2)
           (rectangle (* img 7) h "outline" 'black)
           (rectangle (* img 7) h "solid" col1)))


(: abbrev : Integer Image-Color Image-Color Integer -> Image)
;; forms the day abbreviation row
(define (abbrev x col1 col2 h)
  (beside
   (overlay (text "Sun" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Mon" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Tue" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Wed" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Thu" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Fri" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))
   (overlay (text "Sat" (cast (floor (quotient x 3)) Byte) col2)
            (rectangle x h "outline" 'black)
            (rectangle x h "solid" col1))))

(: n-numbered-cell : Integer Integer Integer Integer Image-Color Image-Color -> Image)
;; outputs a cell depending on the date provided.
(define (n-numbered-cell date month year img bckg font)
  (cond
    [(< date 1) (overlay (square img "outline" "black")
                         (square img "solid" bckg))]
    [(> date (day-to-month (Date month 1 year)))
     (overlay (square img "outline" "black")
              (square img "solid" bckg))]
    [(and
      (< date 7)
      (= (Day-Of-Week-num (find-day-of-week (Date month date year))) 1)
      (= month 9)) (overlay (text (number->string date) (cast (floor (/ img 3)) Byte) font)
                            (square img "outline" "black")
                            (square img "solid" 'red))]
    [(and
      (< date 29)
      (> date 21)
      (= (Day-Of-Week-num (find-day-of-week (Date month date year))) 4)
      (= month 11)) (overlay (text (number->string date) (cast (floor (/ img 3)) Byte) font)
                             (square img "outline" "black")
                             (square img "solid" 'red))]

    [(and
      (> date 24)
      (= (Day-Of-Week-num (find-day-of-week (Date month date year))) 1)
      (= month 5)) (overlay (text (number->string date) (cast (floor (/ img 3)) Byte) font)
                            (square img "outline" "black")
                            (square img "solid" 'red))]
          
    [else (overlay (text (number->string date) (cast (floor (/ img 3)) Byte) font)
                   (square img "outline" "black")
                   (square img "solid" bckg))]))


(: row : Integer Integer Integer Integer Image Integer Image-Color Image-Color -> Image)
;; weeks of the calendar.
(define (row m d y size partimg i bckg font)
  (cond
    [(> i 6) partimg]
    [else (row m (+ d 1) y size
               (beside partimg
                       (n-numbered-cell d m y size bckg font)) (+ i 1) bckg font)]))


(: start-month-date : Integer Integer -> Integer)
;;
(define (start-month-date m y)
  (match (Day-Of-Week-num (find-day-of-week (Date m 1 y)))
    [0 1]
    [1 0]
    [2 -1]
    [3 -2]
    [4 -3]
    [5 -4]
    [_ -5]))

(: month : Integer Integer Integer Image Integer Image-Color Image-Color -> Image)
;; outputs an image of the month
(define (month m y size partimg count bckg font)
  (cond
    [(>= count (day-to-month (Date m 1 y))) partimg]
    [(= 0 count) (month m y size
                        (above partimg
                               (row m (start-month-date m y) y size empty-image 0 bckg font))
                        (+ 6 (start-month-date m y)) bckg font)]   
    [else (month m y size
                 (above partimg (row m (+ count 1) y size empty-image 0 bckg font)) (+ count 7) bckg font)]))


(: draw-month : CalFormat Integer Integer -> Image)
;;
(define (draw-month fmt m y)
  (above (banner m y (CalFormat-cell-size fmt)
                 (CalFormat-title-bar-bg fmt)
                 (CalFormat-title-bar-font fmt)
                 (CalFormat-title-bar-height fmt))
         (abbrev (CalFormat-cell-size fmt)
                 (CalFormat-day-label-bg fmt)
                 (CalFormat-day-label-font fmt)
                 (CalFormat-day-label-height fmt))
         (month m y (CalFormat-cell-size fmt) empty-image 0
                (CalFormat-cell-bg fmt)
                (CalFormat-cell-font fmt))))
                         
(: fmt0 CalFormat)
(define fmt0
  (CalFormat 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30
             'lightyellow 'black))





                  
(: draw-to-calworld : CalWorld -> Image)
;; take the calworld and outputs an image.
(define (draw-to-calworld cal)
  (match cal
    [(CalWorld format month year)
     (draw-month format month year)]))
                
(: react-to-keyboard : CalWorld String -> CalWorld)
;; key reactions for the calendar to respond to
;; when you hit up, the calendar flips a year in advance. when you hit down,
;; the calendar flips down a year. when you hit right, the calendar switches 
;; to the next month, and when you hit left, the calendar switches a month
;; behind.
(define (react-to-keyboard world key)
  (match world
    [(CalWorld format month year)
     (match key
       ["left" (if (= month 1) (CalWorld format 1 (- year 1))
                   (CalWorld format (- month 1) year))]
       ["right" (if (= month 12) (CalWorld format 1 (+ 1 year))
                    (CalWorld format (+ month 1) year))]
       ["up" (CalWorld format month (+ year 1))]
       ["down" (CalWorld format month (- year 1))])]))
                      
(: run : CalFormat Integer Integer -> CalWorld)
;; run function to generate a calendar using the reacting keys
;; and the draw to calworld function.
(define (run format month year)
  (big-bang (CalWorld format month year) : CalWorld
    [to-draw draw-to-calworld]
    [on-key react-to-keyboard]))


(test)
