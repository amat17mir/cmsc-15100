#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require typed/racket/date)

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

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-type Day
  (U 'Su 'M 'Tu 'W 'Th 'F 'Sa))

;; note: for the month, 1 means January, 2 means February, etc.

(define-struct Day-Of-Week
  ([num : Integer]))

;; note: 0 means Sunday, 1 means Monday, ..., 6 means Saturday

(define-struct CalWorld2
  ([mode : (U 'calendar 'help)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date] ;; current date of the user
   [now-date-string : String]
   [now-time : Time]))

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


(: title-bar : Integer Integer Integer Image-Color Image-Color Integer -> Image)
;; produces the title with specified background and text
;; color of a given month and year
(define (title-bar mon yr img col1 col2 h)
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

(: n-numbered-cell : Integer Integer Integer Integer
   Image-Color Image-Color Date -> Image)
;; outputs a cell depending on the date provided.
(define (n-numbered-cell x m y c col1 col2 date)
  (cond
    [(and (>= x 1) (<= x (days-in-month m y)))
          (overlay
            (cond
              [(= x (Date-d date))
               (square c "solid" (color 1 200 70 130))]
              [else empty-image])
           (text (number->string x) 15 col2)(square c "solid" col1)
                   (square c "outline" 'black))]
          
    [(> x (days-in-month m y))  (overlay
                                            (square c "solid" col1)
                                            (square c "outline" 'black))]
    [else (overlay
           (square c "solid" col1)
           (square c "outline" 'black))]))




(: row : Integer Integer Integer Integer Image
   Integer Image-Color Image-Color -> Image)
;; weeks of the calendar.
(define (row m d y size partimg i bckg font)
  (cond
    [(> i 6) partimg]
    [else (row m (+ d 1) y size
               (beside partimg
                       (n-numbered-cell d m y size bckg font))
               (+ i 1) bckg font)]))
 

(: start-month-date : Integer Integer -> Integer)
;;
(define (start-month-date m y)
  (local
    {(define count (Day-Of-Week-num (find-day-of-week (Date m 1 y))))}
    (cond
      [(= count 0) 1]
      [(= count 1) 0]
      [(= count 2) -1]
      [(= count 3) -2]
      [(= count 4) -3]
      [(= count 5) -4]
      [else -5])))



(: month : Integer Integer Integer Image
   Integer Image-Color Image-Color -> Image)
;; outputs an image of the month
(define (month m y size partimg count bckg font)
  (cond
    [(>= count (day-to-month (Date m 1 y))) partimg]
    [(= 0 count)
     (month m y size
            (above partimg
                   (row m (start-month-date m y) y size empty-image 0 bckg font))
            (+ 6 (start-month-date m y)) bckg font)]   
    [else
     (month m y size
            (above partimg (row m (+ count 1) y size empty-image 0 bckg font))
            (+ count 7) bckg font)]))


(: draw-month : CalFormat Integer Integer Date -> Image)
;;
(define (draw-month fmt m y date)
  (above (title-bar m y (CalFormat-cell-size fmt)
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



(: add-month : Date -> Date)
; adds one month.
(define (add-month date)
  (match date
    [(Date month day year)
     (Date (+ month 1) day year)]))

(: subtract-month : Date -> Date)
; subtracts one month.
(define (subtract-month date)
  (match date
    [(Date month day year)
     (Date (- month 1) day year)]))

(: add-year : Date -> Date)
; adds one month.
(define (add-year date)
  (match date
    [(Date month day year)
     (Date month day (+ year 1))]))

(: subtract-year : Date -> Date)
; subtracts one month.
(define (subtract-year date)
  (match date
    [(Date month day year)
     (Date month day (- year 1))]))

(: show-date : Date Time -> Image)
;; represents the time and date as an image.
(define (show-date date time)
  (overlay/align "left" "top"
                 (above
                  (text (date-to-string date) 15 'black)
                  (text (time-to-string time) 15 'black)
                  (text "Press ? for Help." 15 'black))
                 (rectangle 50 50 'solid 'white)))
  
(: react-to-keyboard : CalWorld2 String -> CalWorld2)
; keyboard reactions for the calendar
(define (react-to-keyboard world enter)
  (match world
    [(CalWorld2 mode format current-date now-date str nowtime)
     (match enter
       ["up" (CalWorld2 mode format (add-year current-date)
                        now-date str nowtime)]
       ["down" (CalWorld2 mode format (subtract-year current-date)
                          now-date str nowtime)]
       [(or "=" "+") (CalWorld2 mode format (tomorrow current-date)
                                now-date str nowtime)]
       ["-" (CalWorld2 mode format (yesterday current-date)
                       now-date str nowtime)]
       ["right" (CalWorld2 mode format (add-month current-date)
                           now-date str nowtime)]
       ["left" (CalWorld2 mode format (subtract-month current-date)
                          now-date str nowtime)]
       ["?" (CalWorld2 'help format current-date now-date str nowtime)]
       ["escape" (CalWorld2 'calendar format current-date now-date str
                            nowtime)]
       ["t" (CalWorld2 'calendar format now-date now-date str nowtime)]
       [_ world])]))

(: tick : CalWorld2 -> CalWorld2)
; changes the seconds in the universe
(define (tick world)
  (match world
    [(CalWorld2 mode format current-date now-date str nowtime)
     (Time-second nowtime) (CalWorld2 mode format current-date (read-date-now)
                                      str (read-time-now))]))

(: to-draw-world : CalWorld2 -> Image)
;; returns the image with the calworld function and a second function
;; with the date.
(define (to-draw-world world)
  (match world
    [(CalWorld2 mode format current-date now-date str nowtime)
     (match mode
       ['calendar
        (above
         (beside
          (draw-month format (Date-m current-date) (Date-y current-date)
                      current-date)
      
          (show-date now-date nowtime))
         (date-to-image current-date))]
       ['help (help)])]))


(: run : CalFormat Integer Integer -> CalWorld2)
;;; run function to generate a calendar using the reacting keys
;;; and the draw to calworld function.
(define (run world month year)
  (big-bang (CalWorld2 'calendar world
                       (Date month 1 year)
                       (read-date-now)
                       (date-to-string (read-date-now))
                       (read-time-now)) : CalWorld2
    [to-draw to-draw-world]
    [on-key react-to-keyboard]
    [on-tick tick 1/4]))

(: month-to-string : Integer -> String)
; Function that returns a string value of the month inputted
(define (month-to-string x)
  (cond
    [(= x 1) "January"]
    [(= x 2) "February"]
    [(= x 3) "March"]
    [(= x 4) "April"]
    [(= x 5) "May"]
    [(= x 6) "June"]
    [(= x 7) "July"]
    [(= x 8) "August"]
    [(= x 9) "September"]
    [(= x 10) "October"]
    [(= x 11) "November"]
    [(= x 12) "December"]
    [ else (error "Invalid Month")]))

(: day-of-week-to-string : Integer -> String)
; Takes in an integer and returns a string corresponding to the day of the week
(define (day-of-week-to-string n)
  [match n
    [0 "Sunday"]
    [1 "Monday"]
    [2 "Tuesday"]
    [3 "Wednesday"]
    [4 "Thursday"]
    [5 "Friday"]
    [6 "Saturday"]
    [_ (error "Day is invalid.")]])

(: tomorrow  : Date -> Date)
; Takes in a date and returns the date the following date
(define (tomorrow date)
  (match date
    [(Date month day year)
     (cond
       [(and (= day (days-in-month month year)) (= month 12))
        (Date 1 1 (+ year  1))]
       [(= day (days-in-month month year)) (Date (+ month 1) 1 year)]
       [else (Date month (+ day 1) year)])]))

(: yesterday  : Date -> Date)
;Takes in a date and returns the previous date
(define (yesterday date)
  (match date
    [(Date month day year)
     (cond
       [(and (= day 1) (= month 1)) (Date 12 31 (- year  1))]
       [(= day 1) (Date (- month 1) (days-in-month (- month 1) year) year)]
       [else (Date month (- day 1) year)])]))

(: read-date-now : -> Date)
;Function that returns the current date at any given time
(define (read-date-now)
  (Date (date-month (current-date))
        (date-day (current-date))
        (date-year (current-date))))

(: date-to-image : Date -> Image)
;; converts a date to an image.
(define (date-to-image date)
  (overlay
   (text (date-to-string date) 30 'black)
   (rectangle 80 120 'solid 'white)))

(: read-time-now : -> Time)
; Function that returns the current time at any given time
(define (read-time-now)
  (Time (date-hour (current-date))
        (date-minute (current-date))
        (date-second (current-date))))

(: help : -> Image)
;; forms the day abbreviation row
(define (help)
  (above
   (overlay (text "+                + day" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "-                - day" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "[right]          + month" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "[left]           - month" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "[up]             + year" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "[down]           - year" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "T                today" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "?                help" 20 'black)
            (rectangle 250 50 "outline" 'black)
            (rectangle 250 50 "solid" 'white))
   (overlay (text "Press esc to return to the calendar" 20 'black)
            (rectangle 320 30 "outline" 'black)
            (rectangle 320 30 "solid" 'white))))


(: date-to-string : Date -> String)
;; inputs the date given and then outputs the string form.
(define (date-to-string date)
  (match date
    [(Date m d y)
     (string-append (day-of-week-to-string
                     (Day-Of-Week-num (find-day-of-week date)))
                    ", " (month-to-string m) " " (number->string d)
                    ", " (number->string y))]))

(: time-to-string : Time -> String)
;; inputs the time and outputs the string.
(define (time-to-string time)
  (match time
    [(Time h m s)
     (cond
       [(= h 0)
        (string-append (number->string 12) ":" (number->string m) ":"
                       (number->string s) " am")]
       [(= h 12)
        (string-append (number->string 12) ":" (number->string m) ":"
                       (number->string s) " pm")]
       [(< h 12)
        (string-append (number->string h) ":" (number->string m) ":"
                       (number->string s) " am")]
       [else (string-append (number->string (- h 12)) ":" (number->string m) ":"
                            (number->string s) " pm")])]))


(test)
