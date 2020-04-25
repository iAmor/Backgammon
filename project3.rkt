#lang typed/racket

;; CMSC 15100 Autumn 2019, University of Chicago
;; Project 1 reference solution.
;; Prepared by Adam Shaw Nov 2019.

;; Students are expressly permitted to incorporate this code into
;; their own work in whole or in part, with any degree of modification.

;; Please indicate any uses of this code with comments.


;;NOTE: I used the entirety of the reference solution for my project 2.
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))
; === data definitions

(define-type Player
  (U 'Black 'White))

(define-struct OccupiedPoint
  ([color : Player]
   [count : Integer]))

(define-type Point
  (U OccupiedPoint 'EmptyPoint))

(define-struct Board
  ([points : (Listof Point)]
   [black-bar : Integer]
   [white-bar : Integer]
   [black-off : Integer]
   [white-off : Integer]))

(define-struct Style
  ([checker-radius : Integer]
   [spacing : Integer]
   [black-checker : (Integer -> Image)]
   [white-checker : (Integer -> Image)]
   [dark-point : (Integer Boolean -> Image)]
   [light-point : (Integer Boolean -> Image)]
   [background : (Integer Integer -> Image)]
   [label : (String -> Image)]
   [black-die : (Integer Integer -> Image)]
   [white-die : (Integer Integer -> Image)]))

(define-struct Game
    ([board : Board]
     [turn : Player]
     [moves : (Listof Integer)]))

(define-struct (Pair A B)
  ([item1 : A]
   [item2 : B]))

(define-struct PointNum
   ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
    'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
    'Nowhere))

(define-struct Dice
  ([value1 : Integer]
   [value2 : Integer]))

(define-struct World
  ([game : Game]
   [style : Style]
   [bdice : Dice]
   [wdice : Dice]
   [click1 : BoardLoc]
   [highlight : BoardLoc]
   [history : (Listof Game)]))
         
(define-struct Location
  ([x : Integer]
   [y : Integer]))

;; === general-purpose operations

(: quicksort : (Listof Integer) -> (Listof Integer))
;;taken from piazza, sorts from least to greatest
(define (quicksort nums)
  (match nums
    ['() '()]
    [(cons pivot morenums)
     (append
      (quicksort (filter (lambda ([n : Integer] ) (<= n pivot)) morenums))
      (list pivot)
      (quicksort (filter (lambda ([n : Integer]) (> n pivot)) morenums)))]))

(check-expect (quicksort (list 21 32 13 41)) (list 13 21 32 41)) 

(: guarantee-byte (Integer -> Byte))
;;guarantees that an integer is a byte
(define (guarantee-byte n)
  (if (byte? n) n (error "not a byte")))

(check-expect (guarantee-byte 254) 254)
(check-error (guarantee-byte 257) "not a byte")

(: map-alt : All (A B) (A -> B) (A -> B) (Listof A) -> (Listof B))
;; map two functions over a list, alternating functions
;; (map f g (list a1 a2 a3 a4 ...)) -> (list (f a1) (g a2) (f a3) (g a4) ...)
;; ex: (map-alt add1 sub1 '(1 2 3 4)) -> '(2 1 4 3)
(define (map-alt f g xs)
  (match xs
    ['() '()]
    [(cons first rest) (cons (f first) (map-alt g f rest))]))

(check-expect (map-alt add1 sub1 '(1 2 3 4)) '(2 1 4 3))

(: take : All (a) Integer (Listof a) -> (Listof a))
;; take n items from front of list
(define (take n xs)
  (if (or (<= n 0) (empty? xs))
      '()
      (cons (first xs) (take (sub1 n) (rest xs)))))

(check-expect (take 2 '(a b c d)) '(a b))

(: drop : All (a) Integer (Listof a) -> (Listof a))
;; drop n items from front of list
(define (drop n xs)
  (if (<= n 0)
      xs
      (drop (sub1 n) (rest xs))))

(check-expect (drop 2 '(a b c d)) '(c d))

(: take/drop : All (T) Integer (Listof T) -> (Pair (Listof T) (Listof T)))
;; pair together the results of both taking and dropping
(define (take/drop n xs)
  (Pair (take n xs) (drop n xs)))

(check-expect (take/drop 2 '(a b c d e)) (Pair '(a b) '(c d e)))

(: vspace : Integer -> Image)
;; a rectangle of width 0 is invisible vertical space, regardless of color
(define (vspace length)
  (rectangle 0 length 'solid 'white))

(: hspace : Integer -> Image)
;; a rectangle of height 0 is invisible horiz. space, regardless of color
(define (hspace width)
  (rectangle width 0 'solid 'white))

(: beside/space : Integer (Listof Image) -> Image)
;; assemble images beside one another with space in between
(define (beside/space spacing images)
  (match images
    ['() empty-image]
    [(list i) i]
    [(cons f r) (beside f (hspace spacing) (beside/space spacing r))]))

;; === backgammon-specific widgets and draw-board function

(: draw-checker : Image-Color -> (Integer -> Image))
;; curried function -- given a color, return
;; a function that draws a checker of that color
(define (draw-checker fill-color)
  (lambda ([radius : Integer])
    (overlay (circle radius 'outline 'black)
             (circle radius 'solid fill-color))))

(: draw-point : Image-Color -> (Integer Boolean -> Image))
;; curried function -- given color, return a function
;; that will draw the point (without checkers) given
;; checker radius and up-or-down boolean
(define (draw-point color)
  (lambda ([radius : Integer] [point-up? : Boolean])
    (local
      {(define leg-length (* radius (sqrt 101)))
       (define theta (radians->degrees (* 2 (atan 1/10))))
       (define tri (isosceles-triangle leg-length theta 'solid color))}
      (rotate (if point-up? 0 180) tri))))

(: draw-background : Image-Color Image-Color -> (Integer Integer -> Image))
;; allow five checkers' space between points, vertically
;; allow 3*radius each for the bar and the "off" area
(define (draw-background board-color off-color)
  (lambda ([radius : Integer] [space : Integer])
    (local
      {(define bar-width (* 3 radius))
       (define off-width (* 3 radius))
       (define board-section-width (+ (* 12 radius) (* 5 space)))
       (define height (* 30 radius))}
      (beside 
               (rectangle board-section-width height 'solid board-color)
              (overlay (rectangle 3 height 'solid 'black)
                       (rectangle bar-width height 'solid off-color))
              
               (rectangle board-section-width height 'solid board-color)
              (rectangle off-width height 'solid off-color)))))

(: draw-off-checkers :
   Integer (Integer -> Image) Integer (String -> Image) -> Image)
;; for checkers not on the board, either on the bar or in the "off" area
;; draw a single checker with a number on it
(define (draw-off-checkers n f radius label)
  (if (> n 0)
      (overlay (label (number->string n)) (f radius))
      empty-image))

(: draw-checkers :
   Integer Boolean (Integer -> Image) Integer (String -> Image) -> Image)
;; draws a stack of up to 5 checkers, label with number if more than 5
;; parameters:
;; n   : Integer, a number of checkers
;; up? : Boolean, point direction
;;   (this determines where the numeric label goes if needed)
;; f   : Integer -> Image, a function to draw a checker
;; radius : Integer, checker radius
;; label : String -> Image, a label-making function
(define (draw-checkers n up? f radius label)
  (overlay/align "middle"
                 (if up? "top" "bottom")
                 (if (<= n 5)
                     empty-image
                     (label (number->string n)))
                 (foldr above
                        empty-image
                        (make-list (min 5 n) (f radius)))))

(: draw-black-die : Integer Integer -> Image)
;;draws the black die
(define (draw-black-die radius number)
  (match number
    [0 empty-image]
    [1 (overlay
        (circle 5 'solid 'black)
        (square (* 2 radius) 'solid 'brown))]
[2 (place-image
    (circle 4 'solid 'black)
    (* radius (/ 3 2)) (* radius (/ 3 2))
    (place-image
     (circle 4 'solid 'black)
     (/ radius 2) (/ radius 2)
     (square (* radius 2) 'solid 'brown)))]
[3 (place-image
    (circle 4 'solid 'black)
    (/ radius 2) (* radius (/ 3 2)) (place-image
          (circle 4 'solid 'black)
          (* radius (/ 3 2)) (* radius (/ 3 2))
          (place-image
           (circle 4 'solid 'black)
           radius (/ radius 2)
           (square (* radius 2) 'solid 'brown))))]
[4 (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     (/ radius 2) (* radius (/ 3 2)) (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (* radius (/ 3 2)) (/ radius 2)
            (square (* radius 2) 'solid 'brown)))))]
[5 (place-image
   (circle 3 'solid 'black)
   radius radius
   (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     (/ radius 2) (* radius (/ 3 2)) (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (* radius (/ 3 2)) (/ radius 2)
            (square (* 2 radius) 'solid 'brown))))))]
[6 (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
   (circle 3 'solid 'black)
   (* radius (/ 3 2)) (/ radius 2)
   (place-image
    (circle 3 'solid 'black)
    radius (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     radius (* radius (/ 3 2))
     (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (/ radius 2) (* radius (/ 3 2 ))
            (square (* 2 radius) 'solid 'brown)))))))]))

(: draw-white-die : Integer Integer -> Image)
;;draws the black die
(define (draw-white-die radius number)
  (match number
    [0 empty-image]
    [1 (overlay
        (circle 5 'solid 'black)
        (square (* 2 radius) 'solid 'ivory))]
[2 (place-image
    (circle 4 'solid 'black)
    (* radius (/ 3 2)) (* radius (/ 3 2))
    (place-image
     (circle 4 'solid 'black)
     (/ radius 2) (/ radius 2)
     (square (* radius 2) 'solid 'ivory)))]
[3 (place-image
    (circle 4 'solid 'black)
    (/ radius 2) (* radius (/ 3 2)) (place-image
          (circle 4 'solid 'black)
          (* radius (/ 3 2)) (* radius (/ 3 2))
          (place-image
           (circle 4 'solid 'black)
           radius (/ radius 2)
           (square (* radius 2) 'solid 'ivory))))]
[4 (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     (/ radius 2) (* radius (/ 3 2)) (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (* radius (/ 3 2)) (/ radius 2)
            (square (* radius 2) 'solid 'ivory)))))]
[5 (place-image
   (circle 3 'solid 'black)
   radius radius
   (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     (/ radius 2) (* radius (/ 3 2)) (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (* radius (/ 3 2)) (/ radius 2)
            (square (* 2 radius) 'solid 'ivory))))))]
[6 (place-image
    (circle 3 'solid 'black)
    (/ radius 2) (/ radius 2)
    (place-image
   (circle 3 'solid 'black)
   (* radius (/ 3 2)) (/ radius 2)
   (place-image
    (circle 3 'solid 'black)
    radius (/ radius 2)
    (place-image
     (circle 3 'solid 'black)
     radius (* radius (/ 3 2))
     (place-image
           (circle 3 'solid 'black)
           (* radius (/ 3 2)) (* radius (/ 3 2))
           (place-image
            (circle 3 'solid 'black)
            (/ radius 2) (* radius (/ 3 2 ))
            (square (* 2 radius) 'solid 'ivory)))))))]))

(: roll-die : -> Integer)
;;generates a random number between 1 and 6
(define (roll-die)
  (+ 1 (random 6)))

(check-within (roll-die) 3 3)

(: point-finder1 : Integer Integer Integer Integer -> PointNum)
;;helper functions to find locate points
;;idea given by Riya Sahni
(define (point-finder1 radius x spacing index)
  (if (< (+ (* 10 spacing) (* 25 radius))
            x
            (+ (* 10 spacing) (* 27 radius)))
      (PointNum index)
      (point-finder1 radius (+ (* radius 2) spacing x) spacing (+ index 1))))

(check-expect (point-finder1 14 360 4 1) (PointNum 2))

(: point-finder2 : Integer Integer Integer Integer -> PointNum)
(define (point-finder2 radius x spacing index)
  (if (< (+ (* 5 spacing) (* 10 radius))
         x
         (+ (* 5 spacing) (* 12 radius)))
      (PointNum index)
      (point-finder2 radius (+ (* radius 2) spacing x) spacing (+ index 1))))

(check-expect (point-finder2 14 50 4 7) (PointNum 11))

(: point-finder3 : Integer Integer Integer Integer -> PointNum)
(define (point-finder3 radius x spacing index)
  (if (< (+ (* 5 spacing) (* 10 radius))
         x
         (+ (* 5 spacing) (* 12 radius)))
      (PointNum index)
      (point-finder3 radius (+ (* radius 2) spacing x) spacing (- index 1))))

(check-expect (point-finder3 14 90 4 18) (PointNum 15))

(: point-finder4 : Integer Integer Integer Integer -> PointNum)
(define (point-finder4 radius x spacing index)
  (if (< (+ (* 10 spacing) (* 25 radius))
         x
         (+ (* 10 spacing) (* 27 radius)))
      (PointNum index)
      (point-finder4 radius (+ (* radius 2) spacing x) spacing (- index 1))))

(check-expect (point-finder4 14 340 4 24) (PointNum 22))

(: click-where : Style Integer Integer -> ClickLoc)
;;given a style and the x and y coords, tells where you clicked
(define (click-where style x y)
  (cond
    [(and (< (+ (* 5 (Style-spacing style)) (* 12 (Style-checker-radius style)))
             x
             (+ (* 5 (Style-spacing style)) (* 15 (Style-checker-radius style)))
             )
          (< 0 y (* 10 (Style-checker-radius style))))
     'BlackBar]
    [(and (< (+ (* 5 (Style-spacing style)) (* 12 (Style-checker-radius style)))
             x
             (+ (* 5 (Style-spacing style)) (* 15 (Style-checker-radius style)))
             )
          (< (* 20 (Style-checker-radius style))
             y
             (* 30 (Style-checker-radius style))))
     'WhiteBar]
    [(and (< (+ (* 10 (Style-spacing style)) (* 27 (Style-checker-radius style))
                )
             x
             (+ (* 10 (Style-spacing style)) (* 30 (Style-checker-radius style))
                ))
          (< 0 y (* 10 (Style-checker-radius style))))
     'BlackOff]
    [(and (< (+ (* 10 (Style-spacing style)) (* 27 (Style-checker-radius style))
                )
             x
             (+ (* 10 (Style-spacing style)) (* 30 (Style-checker-radius style))
                ))
          (< (* 20 (Style-checker-radius style))
             y
             (* 30 (Style-checker-radius style))))
     'WhiteOff]
    [(and (< (* 13 (Style-checker-radius style))
             y
             (* 17 (Style-checker-radius style)))
          (< (* 4 (Style-checker-radius style))
             x
             (* 10 (Style-checker-radius style))))
     'WhiteDice]
    [(and (< (* 13 (Style-checker-radius style))
             y
             (* 17 (Style-checker-radius style)))
          (< (* 22 (Style-checker-radius style))
             x
             (* 28 (Style-checker-radius style))))
     'BlackDice]
    [(and (< (+ (* 5 (Style-spacing style))
                (* 15 (Style-checker-radius style)))
             x
             (+ (* 5 (Style-spacing style))
                (* 17 (Style-checker-radius style))))
          (< 0 y (* 10 (Style-checker-radius style))))
     (PointNum 19)]                   
    [(and (< (+ (* 10 (Style-spacing style))
                (* 25 (Style-checker-radius style)))
             x
             (+ (* 10 (Style-spacing style))
                (* 27 (Style-checker-radius style))))
          (< 0 y (* 10 (Style-checker-radius style))))
     (PointNum 24)]
    [(and (< (+ (* 10 (Style-spacing style))
                (* 25 (Style-checker-radius style)))
             x
             (+ (* 10 (Style-spacing style))
                (* 27 (Style-checker-radius style))))
          (< (* 20 (Style-checker-radius style))
             y (* 30 (Style-checker-radius style))))
     (PointNum 1)]
    [(and (< (* 20 (Style-checker-radius style))
             y (* 30 (Style-checker-radius style)))
          (< (+ 20 (* 15 (Style-checker-radius style)))
             x (+ 20 (* 27 (Style-checker-radius style)))))
     (point-finder1 (Style-checker-radius style)
                    x (Style-spacing style) 1)]
    [(and (< (* 20 (Style-checker-radius style))
             y (* 30 (Style-checker-radius style)))
          (< 0 x (+ (* 5 (Style-spacing style))
                    (* 12 (Style-checker-radius style)))))
     (point-finder2 (Style-checker-radius style)
                    x (Style-spacing style) 7)]
    [(and (< 0 y (* 10 (Style-checker-radius style)))
          (< 0 x (+ (* 5 (Style-spacing style))
                    (* 12 (Style-checker-radius style)))))
     (point-finder3 (Style-checker-radius style)
                    x (Style-spacing style) 18)]
    [(and (< 0 y (* 10 (Style-checker-radius style)))
          (< (+ (* 10 (Style-spacing style))
                (* 15 (Style-checker-radius style)))
             x (+ (* 10 (Style-spacing style))
                  (* 27 (Style-checker-radius style)))))
     (point-finder4 (Style-checker-radius style)
                    x (Style-spacing style) 24)]
    [else 'Nowhere]))

(check-expect (click-where (Style 14
         4
         (draw-checker 'brown)
         (draw-checker 'ivory)
         (draw-point 'saddlebrown)
         (draw-point 'moccasin)
         (draw-background (color 40 10 10) (color 80 20 20))
         (lambda ([s : String]) (text s 12 'black))
         draw-black-die
         draw-white-die) 40 30) (PointNum 14))

(check-expect (click-where (Style 14
         4
         (draw-checker 'brown)
         (draw-checker 'ivory)
         (draw-point 'saddlebrown)
         (draw-point 'moccasin)
         (draw-background (color 40 10 10) (color 80 20 20))
         (lambda ([s : String]) (text s 12 'black))
         draw-black-die
         draw-white-die) 120 210) 'WhiteDice)


(: list-setter : All (A) (Listof A) Integer A -> (Listof A))
;;helper function to insert an element into a list
(define (list-setter pts index point)
  (append (take index pts) (cons point (drop (+ 1 index) pts))))

(check-expect (list-setter '(1 2 3 4 5 6) 3 7) '(1 2 3 7 5 6)) 

(: remove : Integer (Listof Integer) -> (Listof Integer))
;;removes an item from a list
(define (remove x xs)
  (match xs
    ['() '()]
    [(cons f r)
     (if (= x f)
         r
         (cons f (remove x r)))]))

(check-expect (remove 1 '(1 2 3 4 5)) '(2 3 4 5))

(: apply-move : Game BoardLoc BoardLoc -> Game)
;;moves a piece
(define (apply-move game firstloc secondloc)
(match game
  [(Game board turn moves)
  (match board
    [(Board points bbar wbar boff woff)
     (match* (firstloc secondloc)
       [('BlackBar (PointNum num))
        (match (list-ref points (- num 1))
          [(OccupiedPoint 'White 1)
           (Game (Board (list-setter points (- num 1) (OccupiedPoint 'Black 1))
                  (sub1 bbar) (add1 wbar) boff woff)
                 'White (remove (distance firstloc secondloc) moves))]
          [(OccupiedPoint 'White count)
           game]
          [_ (Game (Board (list-setter points (- num 1)
                                 (match (list-ref points (- num 1))
['EmptyPoint (OccupiedPoint 'Black 1)]
[(OccupiedPoint color count)
(OccupiedPoint color (add1 count))])) (sub1 bbar) wbar boff woff)
                   'White (remove (distance firstloc secondloc) moves))])]     
       [('WhiteBar (PointNum num))
        (match (list-ref points (- num 1))
          [(OccupiedPoint 'Black 1)
           (Game (Board (list-setter points (- num 1) (OccupiedPoint 'White 1))
                  (add1 bbar) (sub1 wbar) boff woff)
                 'Black (remove (distance firstloc secondloc) moves))]
          [(OccupiedPoint 'Black count)
           game]
          [_ (Game (Board (list-setter points (- num 1)
                                 (match (list-ref points (- num 1))
                               ['EmptyPoint (OccupiedPoint 'White 1)]
                               [(OccupiedPoint color count)
(OccupiedPoint color (add1 count))])) bbar (sub1 wbar) boff woff)
                   'Black (remove (distance firstloc secondloc) moves))])]
       
       [((PointNum num) 'BlackBar)
        (Game (Board (list-setter points (- num 1)
                                  (match (list-ref points (- num 1))
                                               [(OccupiedPoint color 1)
                                                'EmptyPoint]
                                               [(OccupiedPoint color count)
                                                (OccupiedPoint color
                                                               (sub1 count))]
                                               ['EmptyPoint
                                                (error "point was empty")]))
               (add1 bbar) wbar boff woff)
              'White (remove (distance firstloc secondloc) moves))]
       [((PointNum num) 'WhiteBar)
        (Game (Board (list-setter points (- num 1)
                                  (match (list-ref points (- num 1))
                                               [(OccupiedPoint color 1)
                                                'EmptyPoint]
                                               [(OccupiedPoint color count)
                                                (OccupiedPoint color
                                                               (sub1 count))]
                                               ['EmptyPoint
                                                (error "point was empty")]))
               bbar (add1 wbar) boff woff)
              'Black (remove (distance firstloc secondloc) moves))]
       [((PointNum num) 'BlackOff)
        (Game (Board (list-setter points (- num 1)
                                  (match (list-ref points (- num 1))
                                               [(OccupiedPoint color 1)
                                                'EmptyPoint]
                                               [(OccupiedPoint color count)
                                                (OccupiedPoint color
                                                               (sub1 count))]
                                               ['EmptyPoint
                                                (error "point was empty")]))
               bbar wbar (add1 boff) woff)
              'White (remove (distance firstloc secondloc) moves))]
       [((PointNum num) 'WhiteOff)
        (Game (Board (list-setter points (- num 1)
                                  (match (list-ref points (- num 1))
                                               [(OccupiedPoint color 1)
                                                'EmptyPoint]
                                               [(OccupiedPoint color count)
                                                (OccupiedPoint color
                                                               (sub1 count))]
                                               ['EmptyPoint
                                                (error "point was empty")]))
               bbar wbar boff (add1 woff))
              'Black (remove (distance firstloc secondloc) moves))]
       [((PointNum num1) (PointNum num2))
        (match* ((list-ref points (- num1 1)) (list-ref points (- num2 1)))
          [((OccupiedPoint 'Black count) (OccupiedPoint 'White 1))
           (Game (Board (list-setter (list-setter points (- num2 1)
                                            (OccupiedPoint 'Black 1))
                               (- num1 1) (OccupiedPoint 'Black
                                                         (sub1 count)))
                  bbar (add1 wbar) boff woff)
                 'White (remove (distance firstloc secondloc) moves))]
          [((OccupiedPoint 'White count) (OccupiedPoint 'Black 1))
           (Game (Board (list-setter (list-setter points (- num2 1)
                                            (OccupiedPoint 'White 1))
                               (- num1 1) (OccupiedPoint 'White (sub1 count)))
                  (add1 bbar) wbar boff woff)
                 'Black (remove (distance firstloc secondloc) moves))]
          [((OccupiedPoint 'Black count) (OccupiedPoint 'White count))
           game]
          [((OccupiedPoint 'White count) (OccupiedPoint 'Black count))
           game]
        [(_ _) (Game (Board (list-setter (list-setter points (- num1 1)
                                                (match
                                                    (list-ref points (- num1 1))
[(OccupiedPoint color 1) 'EmptyPoint]
[(OccupiedPoint color count)
(OccupiedPoint color (sub1 count))]
['EmptyPoint (error "point was empty")]))
                            (- num2 1) (match (list-ref points (- num2 1))
                                         ['EmptyPoint
                                          (match
                                              (list-ref points (- num1 1))
                                                     [(OccupiedPoint 'Black _)
                                                     (OccupiedPoint 'Black 1)]
                                                     [(OccupiedPoint 'White _)
                                                     (OccupiedPoint 'White 1)])]
                                         [(OccupiedPoint color count)
                                          (OccupiedPoint color (add1 count))]))
                      bbar wbar boff woff)
                     'White (remove (distance firstloc secondloc) moves))])]
       [(_ _) (error "invalid move")])])]))

(: distance : BoardLoc BoardLoc -> Integer)
;;calculates the distance between two BoardLocs
(define (distance start end)
  (match* (start end)
    [((PointNum p) (PointNum q))
     (abs(- q p))]
    [('WhiteBar (PointNum n))
     (- 25 n)
     ]
    [('BlackBar (PointNum n))
     n]
    [((PointNum n) 'WhiteOff)
     n]
    [((PointNum n) 'BlackOff)
     (- 25 n)]
     ))

(check-expect (distance (PointNum 1) (PointNum 3)) 2)
(check-expect (distance (PointNum 3) 'WhiteOff) 3)

(: in-list? : Integer (Listof Integer) -> Boolean)
;;checks if something belongs to a list
(define (in-list? x xs)
  (match xs
    ['() #f]
    [(cons f r) (if (= f x)
                    #t
                    (in-list? x r))]))

(check-expect (in-list? 3 '(1 2 3 4)) #t)
(check-expect (in-list? 3 '(4 5 6)) #f)

(: valid-distance? : Game BoardLoc BoardLoc -> Boolean)
;;checks if the distance is valid
(define (valid-distance? game org dest)
  (match game
    [(Game board turn moves)
     (in-list? (distance org dest) moves)]))

(check-expect (valid-distance?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) (PointNum 1) (PointNum 4)) #t)

(: free-point? : Game PointNum -> Boolean)
;;checks if the chosen point can be moved to
(define (free-point? game dest)
  (match game
    [(Game board 'White moves)
     (match board
       [(Board points _ _ _ _)
        (match (list-ref points (- (PointNum-num dest) 1))
          [(or 'EmptyPoint (OccupiedPoint 'White _) (OccupiedPoint 'Black 1))
           #t]
          [(OccupiedPoint 'Black _) #f])])]
    [(Game board 'Black moves)
     (match board
       [(Board points _ _ _ _)
        (match (list-ref points (- (PointNum-num dest) 1))
          [(or 'EmptyPoint (OccupiedPoint 'Black _) (OccupiedPoint 'White 1))
           #t]
          [(OccupiedPoint 'White _) #f])])]))

(check-expect (free-point? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'White (list 2 3)) (PointNum 2)) #t)

(check-expect (free-point? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'White (list 2 3)) (PointNum 1)) #f)

(: valid-point? : Game PointNum -> Boolean)
;; checks if the origin point is valid
(define (valid-point? game org)
  (match game
    [(Game board 'White moves)
     (match board
       [(Board points _ _ _ _)
        (match (list-ref points (- (PointNum-num org) 1))
          [(OccupiedPoint 'White n)
           (> n 0)]
          [_ #f])])]
    [(Game board 'Black moves)
     (match board
       [(Board points _ _ _ _)
        (match (list-ref points (- (PointNum-num org) 1))
          [(OccupiedPoint 'Black n)
           (> n 0)]
          [_ #f])])]))

(check-expect (valid-point? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) (PointNum 1)) #t)

(check-expect (valid-point? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) (PointNum 2)) #f)


(: valid-bearoff? : Game -> Boolean)
;;checks if the a player can bear off
(define (valid-bearoff? game)
  (match game
    [(Game board 'White moves)
     (match board
       [(Board points bbar wbar boff woff)
        (local
          {(define i 7)
           (: loop : Integer (Listof Point) -> Boolean)
           (define (loop index points)
             (if (> 25 index)
                 (match (list-ref points (sub1 index))
                   [(or 'EmptyPoint (OccupiedPoint 'Black _))
                    (loop (add1 index) points)]
                   [(OccupiedPoint 'White _) #f])
                 #t))}
          (and (= 0 wbar) (loop i points)))])]
    [(Game board 'Black moves)
     (match board
       [(Board points bbar wbar boff woff)
        (local
          {(define i 1)
           (: loop : Integer (Listof Point) -> Boolean)
           (define (loop index points)
             (if (> 19 index)
                 (match (list-ref points (sub1 index))
                   [(or 'EmptyPoint (OccupiedPoint 'Black _))
                    (loop (add1 index) points)]
                   [(OccupiedPoint 'White _) #f])
                 #t))}
          (and (= 0 bbar) (loop i points)))])]))

(check-expect (valid-bearoff?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3))) #f)



(: empty-bar? : Game -> Boolean)
;;checks if the player's bar is empty
(define (empty-bar? game)
  (match game
    [(Game board 'White moves)
     (match board
       [(Board points bbar 0 boff woff) #t]
       [(Board points bbar _ boff woff) #f])]
    [(Game board 'Black moves)
     (match board
       [(Board points 0 wbar boff woff) #t]
       [(Board points _ wbar boff woff) #f])]))

(check-expect (empty-bar? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3))) #t)

(check-expect (empty-bar? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         1 0 0 0) 'Black (list 2 3))) #f)

(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;;checks if the given move is legal in the given situation
(define (legal-move? game org dest)
(if (valid-distance? game org dest)
  (match game
    [(Game board turn moves)
     (match* (org dest)
       [((PointNum p) (PointNum q))
        (and (empty-bar? game) (valid-point? game org) (free-point? game dest))]
       [('WhiteBar (PointNum n))
        (match turn
          ['White (free-point? game dest)]
          ['Black #f])]
       [('BlackBar (PointNum n))
        (match turn
          ['Black (free-point? game dest)]
          ['White #f])]
       [((PointNum w) 'WhiteOff)
        (match turn
          ['White (valid-bearoff? game)]
          ['Black #f])]
       [((PointNum b) 'BlackOff)
        (match turn
          ['Black (valid-bearoff? game)]
          ['White #f])]
       [(_ _) #f])])
  #f))

(check-expect (legal-move? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) (PointNum 1) (PointNum 3)) #t)

(check-expect (legal-move? (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) (PointNum 19) (PointNum 23)) #f)

(check-expect (legal-move? (Game (Board (list
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 3)
 (OccupiedPoint 'White 3)
 (OccupiedPoint 'White 3)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 3)
 (OccupiedPoint 'Black 3)
 (OccupiedPoint 'Black 3)) 0 0 0 0) 'White (list 2 3)) (PointNum 2) 'WhiteOff)
              #t)

(: blocked-point? : Game Integer -> Boolean)
;;check if a point is blocked, i.e a point of the opposite color cannot go there
(define (blocked-point? game index)
  (match game
    [(Game board turn moves)
     (match board
       [(Board points bbar wbar boff woff)
        (match (list-ref points index)
          [(OccupiedPoint turn n)
           (> n 1)]
          [_ #f])])]))

(check-expect (blocked-point?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) 0) #t)

(check-expect (blocked-point?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3)) 1) #f)
         

(: occupied-home? : Game -> Boolean)
;;checks if a player can move pieces off their bar
(define (occupied-home? game)
  (match game
    [(Game board turn moves)
     (match turn
       ['White
        (match board
          [(Board points bbar wbar boff woff)
           (local
             {(define i 18)
              (: loop : Integer (Listof Point) -> Boolean)
              (define (loop index pts)
                (if (> 24 index)
                    (if (blocked-point? game index)
                        (loop (add1 index) points)
                        #f)
                    #t))}
             (loop i points))])]
       ['Black
        (match board
          [(Board points bbar wbar boff woff)
           (local
             {(define i 0)
              (: loop : Integer (Listof Point) -> Boolean)
              (define (loop index pts)
                (if (> 6 index)
                    (if (blocked-point? game index)
                        (loop (add1 index) points)
                        #f)
                    #t))}
             (loop i points))])])]))

(check-expect (occupied-home?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3))) #f)

(check-expect (occupied-home? (Game (Board (list
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 2)
 (OccupiedPoint 'White 3)
 (OccupiedPoint 'White 3)
 (OccupiedPoint 'White 3)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 2)
 (OccupiedPoint 'Black 3)
 (OccupiedPoint 'Black 3)
 (OccupiedPoint 'Black 3)) 0 0 0 0) 'White (list 2 3))) #t)

(: occpoints : Game -> (Listof Integer))
;;takes in a game and returns a list of the indexes of
;;the occupied points of the player whose turn it is
(define (occpoints game)
  (match game
    ([Game board 'White moves]
     (match board
       [(Board points bbar wbar boff woff)
        (local
          {(: loop : Integer (Listof Point) -> (Listof Integer))
           (define (loop i points)
             (if (> 24 i)
                 (match (list-ref points i)
                   [(OccupiedPoint 'White n)
                    (cons (add1 i) (loop (add1 i) points))]
                   [_ (loop (add1 i) points)])
                 '()))}
          (loop 0 points))]))
    ([Game board 'Black moves]
     (match board
       [(Board points bbar wbar boff woff)
        (local
          {(: loop : Integer (Listof Point) -> (Listof Integer))
           (define (loop i points)
             (if (> 24 i)
                 (match (list-ref points i)
                   [(OccupiedPoint 'Black n)
                    (cons (add1 i) (loop (add1 i) points))]
                   [_ (loop (add1 i) points)])
                 '()))}
          (loop 0 points))]))))

(check-expect (occpoints (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 2 3))) (list 1 12 17 19))

(: greatest : (Listof Integer) -> Integer)
;;returns the greatest element of a list of integers
(define (greatest xs)
  (last (quicksort xs)))

(check-expect (greatest (list 1 9 38 4 0 57 3 13 29)) 57)
(check-expect (greatest (list 5 4 3)) 5)

(: least : (Listof Integer) -> Integer)
;;returns the least element of a list of integers
(define (least xs)
  (first (quicksort xs)))

(check-expect (least (list 1 2 3 4)) 1)
(check-expect (least (list 3 23 394 20 8482 )) 3)

(: dvalues : (Listof Game) -> (Listof Integer))
;;gets the dice values given a history list
(define (dvalues history)
  (cond
    [(< 2 (length history))
     (local
       {(define astate (first history))
        (define bstate (second history))
        (define cstate (third history))}
       (match astate
         [(Game board turn moves)
          (match (length moves)
            [(or 3 4) (make-list 2 (first moves))]
            [2 moves]
            [1 (match bstate
                 [(Game board turn moves) moves])]
            [0 (match cstate
                 [(Game board turn moves) moves])])]))]
    [(= 2 (length history))
     (match history
       [(list a b)
        (match a
          [(Game board turn moves)
        (match (length moves)
          [(or 3 4) (make-list 2 (first moves))]
          [2 moves]
          [(or 0 1) (match b
                      [(Game board turn moves) moves])])])])]
    [else
     (match history
       [(list a)
        (match a
          [(Game board turn moves)
        (match (length moves)
           [(or 3 4) (make-list 2 (first moves))]
           [_ moves])])])]))
           
           
  
            

(: available-moves? : Game -> Boolean)
;;checks if there are any moves left for a turn
(define (available-moves? game)
  (match game
    [(Game board turn moves)
       (match turn
          ['White
           (match board
             [(Board points bbar wbar boff woff)
              (cond
                [(not (empty-bar? game))
                  (not (occupied-home? game))]
                [(valid-bearoff? game)
                 (match (occpoints game)
                   [opoints
                    (match moves
                      [(list a b)
                       (if (> a b)
                           (<= (- 24 (greatest opoints)) a)
                           (<= (- 24 (greatest opoints)) b))]
                      [(cons a rest)
                       (>= (- 24 (greatest opoints)) a)])])]
              [else (match (occpoints game)
                      [opoints
                       (match moves
                         ['() #f]
                         [(list a)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (legal-move? game
                                                    (PointNum
                                                     (list-ref opoints i))
                                                    (PointNum
                                                     (+ a (list-ref opoints i)))
                                                    )
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))]
                         [(list a b)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (or (legal-move? game
                                                        (PointNum
                                                         (list-ref opoints i))
                                                        (PointNum
                                                         (+
                                                          a
                                                          (list-ref opoints i)
                                                          )))
                                           (legal-move? game
                                                        (PointNum
                                                         (list-ref opoints i))
                                                        (PointNum
                                                         (
                                                          +
                                                          b
                                                          (list-ref opoints i)
                                                          ))))
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))]
                         [(list a b c)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (legal-move? game (PointNum
                                                          (list-ref opoints i))
                                                    (PointNum
                                                     (+ b (list-ref opoints i))
                                                     ))
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))]
                         [(list a b c d)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (legal-move? game (PointNum
                                                          (list-ref opoints i))
                                                    (PointNum
                                                     (+ b (list-ref opoints i)))
                                                    )
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))])])])])]
         ['Black
           (match board
             [(Board points bbar wbar boff woff)
              (cond
                [(not (empty-bar? game))
                  (not (occupied-home? game))]
                [(valid-bearoff? game)
                 (match (occpoints game)
                   [opoints
                    (match moves
                      [(list a b)
                       (if (> a b)
                           (<= (least opoints) a)
                           (<= (least opoints) b))]
                      [(cons a b)
                       (<= (least opoints) a)])])]
              [else (match (occpoints game)
                      [opoints
                       (match moves
                         [(list a b)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (or (legal-move? game
                                                        (PointNum
                                                         (list-ref opoints i))
                                                        (PointNum
                                                         (+ a
                                                            (list-ref opoints i)
                                                            )))
                                           (legal-move? game
                                                        (PointNum
                                                         (list-ref opoints i))
                                                        (PointNum
                                                         (+ b
                                                            (list-ref opoints i)
                                                            ))))
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))]
                         [(list a b c d)
                          (local
                            {(: loop : Integer -> Boolean)
                             (define (loop i)
                               (if (> (length opoints) i)
                                   (if (legal-move? game
                                                    (PointNum
                                                     (list-ref opoints i))
                                                    (PointNum
                                                     (+ b (list-ref opoints i)
                                                        )))
                                       #t
                                       (loop (add1 i)))
                                   #f))}
                            (loop 0))])])])])])]))

(check-expect (available-moves?
               (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2))) 0 0 0 0)
                     'Black (list 2 5))) #t)

(: game-over? : Game -> Boolean)
;;checks if a game is over
(define (game-over? game)
  (match game
    [(Game board turn moves)
     (match board
       [(Board points bbar wbar boff 15) #t]
       [(Board points bbar wbar 15 woff) #t]
       (_ #f))]))

(: winner : Game -> Player)
;;determines who the winner is
(define (winner game)
  (match game
    [(Game board turn moves)
     (match board
       [(Board points bbar wbar boff 15) 'White]
       [(Board points bbar wbar 15 woff) 'Black])]))

(: undo : World -> World)
;;undoes a move
(define (undo world)
  (match world
    [(World game style bdice wdice click1 highlight history)
     (World (first history) style bdice wdice click1 highlight
            (take 1 history))]))
             
    
     
     
    

;; ======== Click Handling =========

;;be sure to roll dice correctly to enable turn functionality

(: sameloc : BoardLoc BoardLoc -> Boolean)
;;deselects if you click the same location twice
(define (sameloc loc1 loc2)
  (match* (loc1 loc2)
    [('BlackBar 'BlackBar) #t]
    [('WhiteBar 'WhiteBar) #t]
    [((PointNum n) (PointNum m)) (= n m)]))
     
     

(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;;click handling function
(define (react-to-mouse world x y event)
  (match event
    ["button-down"
     (if (game-over? (World-game world))
         world
     (match world
       [(World (Game board 'White moves) style bdice wdice 'Nowhere 'Nowhere
               history)
        (match (click-where (World-style world) x y)
          ['BlackBar world]
          ['WhiteBar (World (Game board 'White moves) style bdice wdice
                            'WhiteBar 'WhiteBar history)]
          ['BlackOff world]
          ['WhiteOff world]
          [(PointNum num)
           (World (Game board 'White moves) style bdice wdice
                  (PointNum num) (PointNum num) history)]
          ['WhiteDice
           (local
             {(define a (roll-die))
              (define b (roll-die))}
             (World (Game board 'White (if (= a b)
                                           (make-list 4 a)
                                           (list a b))) style bdice
               (Dice a b) 'Nowhere 'Nowhere (cons (World-game world) history)))]
          ['BlackDice
           (local
             {(define a (roll-die))
              (define b (roll-die))}
             (World (Game board 'Black (if (= a b)
                                           (make-list 4 a)
                                           (list a b))) style (Dice a b)
                    wdice 'Nowhere 'Nowhere (cons (World-game world) history)))]
          ['Nowhere world])]
       [(World (Game board 'Black moves) style bdice wdice
               'Nowhere 'Nowhere history)
        (match (click-where (World-style world) x y)
          ['BlackBar (World (Game board 'Black moves) style bdice wdice
                            'BlackBar 'BlackBar history)]
          ['WhiteBar world]
          ['BlackOff world]
          ['WhiteOff world]
          [(PointNum num)
           (World (Game board 'Black moves) style bdice wdice
                  (PointNum num) (PointNum num) history)]
          ['WhiteDice
           (local
             {(define a (roll-die))
              (define b (roll-die))}
             (World (Game board 'White (if (= a b)
                                           (make-list 4 a)
                                           (list a b))) style bdice
                    (Dice a b) 'Nowhere 'Nowhere history))]
          ['BlackDice
           (local
             {(define a (roll-die))
              (define b (roll-die))}
             (World (Game board 'Black (if (= a b)
                                           (make-list 4 a)
                                           (list a b))) style (Dice a b)
                    wdice 'Nowhere 'Nowhere history))]
          ['Nowhere world])]
       [(World (Game board turn moves) style bdice wdice
               location highlight history)
        (match (click-where (World-style world) x y)
          ['BlackBar (if (sameloc 'BlackBar location)
                         (World (Game board turn moves) style bdice wdice
                                'Nowhere 'Nowhere history)
                         (World (Game board turn moves) style bdice wdice
                                location highlight
                                (cons (World-game world) history)))]
          ['WhiteBar (if (sameloc 'WhiteBar location)
                         (World (Game board turn moves) style bdice wdice
                                'Nowhere 'Nowhere history)
                         (World (Game board turn moves) style bdice wdice
                                location highlight
                                (cons (World-game world) history)))]
          ['BlackOff (if (legal-move? (Game board turn moves)
                                      location 'BlackOff)
                         (World (if (available-moves?
                                     (apply-move (Game board turn moves)
                                                 location 'BlackOff))
                                    (apply-move (Game board 'Black moves)
                                                location 'BlackOff)
                                    (apply-move (Game board 'White moves)
                                                location 'BlackOff))
                                
                                style bdice wdice 'Nowhere 'Nowhere
                                (cons (World-game world) history))
                         world)]
          ['WhiteOff (if (legal-move? (Game board turn moves)
                                      location 'WhiteOff)
                         (World (if (available-moves?
                                     (apply-move (Game board turn moves)
                                                 location 'WhiteOff))
                                    (apply-move (Game board 'White moves)
                                                location 'WhiteOff)
                                    (apply-move (Game board 'Black moves)
                                                location 'WhiteOff))
                                    
                                style bdice wdice 'Nowhere 'Nowhere
                                (cons (World-game world) history))
                         world)]
          [(PointNum num) (if (sameloc (PointNum num) location)
                              (World (Game board turn moves) style bdice wdice
                                     'Nowhere 'Nowhere history)
                               (if (legal-move? (Game board turn moves)
                                                location (PointNum num))
                              (World (if (available-moves?
                                          (apply-move (Game board turn moves)
                                                      location (PointNum num)))
                                         (match turn
                                           ['White (apply-move
                                                    (Game board 'White moves)
                                                    location (PointNum num))]
                                           ['Black (apply-move
                                                    (Game board 'Black moves)
                                                    location (PointNum num))])
                                         (match turn
                                           ['White (apply-move
                                                    (Game board 'Black moves)
                                                    location (PointNum num))]
                                           ['Black (apply-move
                                                    (Game board 'White moves)
                                                    location (PointNum num))]))
                                     style bdice wdice 'Nowhere 'Nowhere
                                     (cons (World-game world) history)
                                     )
                              world))]
          [_ world])]))]
    [_ world]))  

(: key : World String -> World)
;;react to key function
(define (key w k)
  (match k
    ["s" (begin
           (save-game! w)
           w)]
    ["l" (begin
           (load-game (match w
                        [(World _ style _ _ _ _ _)
                         style])))]
    ["u" (undo w)]))



(: draw-board : Style Board -> Image)
;; draw backgammon board given data definitions above
(define (draw-board style board)
  (match style
    [(Style rad spc blc whc dkp ltp bg label bld whd)
     (match board
       [(Board points bbar wbar boff woff)
        (local
          {(: pt : (Integer Boolean -> Image) Boolean -> (Point -> Image))
           ;; generalization of dark point and light point functions
           ;; parameters:
           ;; mkp : (Integer Boolean -> Image), a point-drawing function
           ;; up? : point direction
           (define (pt mkp up?)
             (lambda ([p : Point])
               (match p
                 ['EmptyPoint (mkp rad up?)]
                 [(OccupiedPoint p n)
                  (overlay/align
                   "middle"
                   (if up? "bottom" "top")
                   (draw-checkers n up?
                                  (match p ['Black blc] [_ whc]) rad label)
                   (mkp rad up?))])))}
          (match (take/drop 12 (reverse points))
            [(Pair back12 front12)
             (overlay
              (above
               (match (take/drop 6 (reverse back12))
                 [(Pair back-front6 back-back6)
                  (beside
                   (beside/space spc (map-alt (pt dkp #f)
                                              (pt ltp #f) back-front6))
                   (overlay (draw-off-checkers bbar blc rad label)
                            (hspace (* 3 rad)))
                   (beside/space spc (map-alt (pt dkp #f)
                                              (pt ltp #f) back-back6))
                   (overlay (draw-off-checkers boff blc rad label)
                            (hspace (* 3 rad))))])
               (vspace (* 10 rad))
               (match (take/drop 6 front12)
                 [(Pair front-back6 front-front6)
                  (beside
                   (beside/space spc (map-alt (pt ltp #t)
                                              (pt dkp #t) front-back6))
                   (overlay (draw-off-checkers wbar whc rad label)
                            (hspace (* 3 rad)))
                   (beside/space spc (map-alt (pt ltp #t)
                                              (pt dkp #t) front-front6))
                   (overlay (draw-off-checkers woff whc rad label)
                            (hspace (* 3 rad))))]))
              (bg rad spc))]))])]))

(: draw-dice : World -> Image)
;;helper function that draws all dice
(define (draw-dice world)
  (match world
    [(World (Game board turn moves) style bdice wdice click1 highlight history)
     (match* (style bdice wdice)
       [((Style radius spacing _ _ _ _ _ _ _ _) (Dice value1 value2)
                                                (Dice value3 value4))
        (beside
         (if (= 0 value3)
             empty-image
         (draw-white-die radius value3))
         (hspace 6)
         (if (= 0 value3)
             empty-image
         (draw-white-die radius value4))
         (hspace (* 13 radius))
         (if (= 0 value3)
             empty-image
         (draw-black-die radius value1))
         (hspace 6)
         (if (= 0 value3)
             empty-image
         (draw-black-die radius value2)))])]))

(: draw-highlight : World -> Image)
;;highlights a clicked-on BoardLoc
(define (draw-highlight world)
  (match world
    [(World (Game board turn moves) style bdice wdice click1 highlight history)
     (match style
       [(Style radius spacing _ _ _ _ _ _ _ _)
        (match highlight
          ['Nowhere empty-image]
          ['BlackBar (place-image (rectangle (* 3 radius)
                                             (* 10 radius) 'outline 'cyan)
                                  (+ (* 5 spacing) (* 13.5 radius)) (* 5 radius)
                                  (rectangle (abs (+ (* 30 radius)
                                                     (* 10 spacing)))
                                             (abs (* 30 radius)) 'solid
                                             (color 0 0 0 0)))]
          ['WhiteBar (place-image (rectangle (* 3 radius) (* 10 radius)
                                             'outline 'cyan)
                                  (+ (* 5 spacing) (* 13.5 radius))
                                  (* 25 radius)
                                  (rectangle (abs (+ (* 30 radius)
                                                     (* 10 spacing)))
                                             (abs (* 30 radius)) 'solid
                                             (color 0 0 0 0)))]
          ['WhiteOff (place-image (rectangle (* 3 radius) (* 10 radius)
                                             'outline 'cyan)
                                  (+ (* 10 spacing) (* 27 radius))
                                  (* 25 radius)
                                  (rectangle (abs (+ (* 30 radius)
                                                     (* 10 spacing)))
                                             (abs (* 30 radius))
                                             'solid (color 0 0 0 0)))]
          ['BlackOff (place-image (rectangle (* 3 radius) (* 10 radius)
                                             'outline 'cyan)
                                  (+ (* 10 spacing) (* 27 radius))
                                  (* 25 radius)
                                  (rectangle (abs (+ (* 30 radius)
                                                     (* 10 spacing)))
                                             (abs (* 30 radius))
                                             'solid (color 0 0 0 0)))]
          [(PointNum num) (cond
                            [(<= 1 num 6)
                             (place-image (rectangle (* 2 radius) (* 10 radius)
                                                     'outline 'cyan)
                                          (+ (* (- 10 (sub1 num)) spacing)
                                             (* (- 28 (* 2 num)) radius))
                                          (* 25 radius)
                                          (rectangle (abs (+ (* 30 radius)
                                                             (* 10 spacing)))
                                                     (abs (* 30 radius))
                                                     'solid (color 0 0 0 0)))]
                            [(<= 7 num 12)
                             (place-image (rectangle (* 2 radius) (* 10 radius)
                                                     'outline 'cyan)
                                          (+ (* (- 10 (- num 2)) spacing)
                                             (* (- 25 (* 2 num)) radius))
                                          (* 25 radius)
                                          (rectangle (abs (+ (* 30 radius)
                                                             (* 10 spacing)))
                                                     (abs (* 30 radius))
                                                     'solid (color 0 0 0 0)))]
                            [(<= 13 num 18)
                             (place-image (rectangle (* 2 radius) (* 10 radius)
                                                     'outline 'cyan)
                                          (+ (* (- num 13) spacing)
                                             (* (- (* 2 num) 25) radius))
                                          (* 5 radius)
                                          (rectangle (abs (+ (* 30 radius)
                                                             (* 10 spacing)))
                                                     (abs (* 30 radius))
                                                     'solid (color 0 0 0 0)))]
                            [(<= 19 num 24)
                             (place-image (rectangle (* 2 radius) (* 10 radius)
                                                     'outline 'cyan)
                                          (+ (* (- num 14) spacing)
                                             (* (- (* 2 num) 22) radius))
                                          (* 5 radius)
                                          (rectangle (abs (+ (* 30 radius)
                                                             (* 10 spacing)))
                                                     (abs (* 30 radius))
                                                     'solid (color 0 0 0 0)))]                           
                            [else empty-image])])])])) 

(: draw-winner : World -> Image)
;;overlays a message of the winner on a won game
(define (draw-winner world)
  (if (game-over? (World-game world))
      (match (winner (World-game world))
        ['White (text "WHITE WINS" 20 'white)]
        ['Black (text "BLACK WINS" 20 'white)])
      empty-image))

(: draw : World -> Image)
;;function that draws the World
(define (draw world)
  (overlay (draw-winner world)
           (draw-highlight world)
           (overlay (draw-dice world)
           (draw-board (World-style world) (Game-board (World-game world))))))

(: initworld : Style -> World)
;;helper function that generates the initial state of the world
(define (initworld style)
  (local
    {(define a (roll-die))
     (define b (roll-die))}
    (cond
      [(< a b) (World (Game start-board 'White (list a b)) style
                      (Dice a 0) (Dice b 0) 'Nowhere 'Nowhere '())]
      [(> a b) (World (Game start-board 'Black (list a b)) style
                      (Dice a 0) (Dice b 0) 'Nowhere 'Nowhere '())]
      [else (initworld style)])))

(: run : Style -> World)
;;runs the world with the given style
(define (run style)
  (big-bang (initworld style) : World
    [to-draw draw]
    [on-mouse react-to-mouse]
    [on-key key]))

;; === saving and loading functions

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
      (error "string->integer: invalid integer"))))

(: point->string : Point -> String)
;;serializes a point
(define (point->string point)
  (match point
    ['EmptyPoint "_"]
    [(OccupiedPoint 'White n)
     (string-append "W" (number->string n))]
    [(OccupiedPoint 'Black n)
     (string-append "B" (number->string n))]))

(check-expect (point->string 'EmptyPoint) "_")
(check-expect (point->string (OccupiedPoint 'Black 5)) "B5")

(: points->string : (Listof Point) -> String)
;;converts a list of points into a string
(define (points->string pts)
  (match pts
    ['() ""]
    [(cons f r)
     (string-trim (string-append (point->string f) " " (points->string r)))]))

(check-expect (points->string (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2))))
              "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2")

(: board->string : Board -> String)
;;converts a board into a string
(define (board->string board)
  (match board
    [(Board points bbar wbar boff woff)
     (string-append
      (points->string points) "|" (number->string bbar)
      "|" (number->string wbar) "|" (number->string boff)
      "|" (number->string woff))]))

(check-expect (board->string (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0))
            "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0")

(: numstring : (Listof Integer) -> String)
;;helper function to convert the moves list into a string
(define (numstring moves)
  (match moves
    ['() ""]
    [(cons f r)
     (string-trim (string-append (number->string f) " " (numstring r)))]))

(check-expect (numstring (list 1 2 3 4 5)) "1 2 3 4 5")

(: game->string : Game -> String)
;;converts a game into a string
(define (game->string game)
  (match game
    [(Game board turn moves)
     (string-append
      (board->string board)
      "@"
      (match turn
        ['Black "B"]      
        ['White "W"])
      "@"
      (string-trim (numstring moves)))]))

(check-expect (game->string (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 4 6)))
"B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@B@4 6")
              

(: history->string : (Listof Game) -> String)
;;converts a history list to a string
(define (history->string history)
  (match history
    ['() ""]
    [(cons f r)
     (local
       {(define str (string-append(game->string f) "!" (history->string r)))
        }
       (substring str 0 (sub1 (string-length str))))]))

(check-expect (history->string
               (list (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 4 5))
                     (Game (Board (list
 (OccupiedPoint 'Black 2)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'White 5)
 'EmptyPoint
 (OccupiedPoint 'White 3)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 4)
 (OccupiedPoint 'White 5)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 4)
 'EmptyPoint
 (OccupiedPoint 'Black 5)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'White 2)) 0 0 0 0) 'Black (list 4))))
(string-append "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0"
"@B@4 5!B2 _ _ _ _ W5 _ W3 _ _ _ B4 W5 _ _ _ B4 _ B5 _ _ _ _ W2|0|0|0|0@B@"))

(: string->point : String -> Point)
;;converts a string back into a point
(define (string->point pstring)
  (match pstring
    ["_" 'EmptyPoint]
    [_ (local
         {(define s (string->list pstring))
          (define a (first s))
          (define b (string (second s)))}
         (match a
           [#\B (OccupiedPoint 'Black (string->integer b))]
           [#\W (OccupiedPoint 'White (string->integer b))]))]))

(check-expect (string->point "_") 'EmptyPoint)
(check-expect (string->point "W4") (OccupiedPoint 'White 4))

(: string->points : String -> (Listof Point))
;;deserializes a list of points
(define (string->points pts)
  (map string->point (string-split pts)))

(check-expect
 (string->points "B2 _ _ _ _ W5 _ W3 _ _ _ B4 W5 _ _ _ B4 _ B5 _ _ _ _ W2")
 (list
 (OccupiedPoint 'Black 2)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'White 5)
 'EmptyPoint
 (OccupiedPoint 'White 3)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 4)
 (OccupiedPoint 'White 5)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'Black 4)
 'EmptyPoint
 (OccupiedPoint 'Black 5)
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 'EmptyPoint
 (OccupiedPoint 'White 2)))

(: string->board : String -> Board)
;;converts a string into a board
(define (string->board board)
  (local
    {(define bd (string-split board "|"))}
    (Board (string->points (first bd)) (string->integer (second bd))
           (string->integer (third bd)) (string->integer (fourth bd))
           (string->integer (fifth bd)))))

(check-expect (string->board
              "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0")
               (Board
 (list
  (OccupiedPoint 'Black 2)
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  (OccupiedPoint 'White 5)
  'EmptyPoint
  (OccupiedPoint 'White 3)
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  (OccupiedPoint 'Black 5)
  (OccupiedPoint 'White 5)
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  (OccupiedPoint 'Black 3)
  'EmptyPoint
  (OccupiedPoint 'Black 5)
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  'EmptyPoint
  (OccupiedPoint 'White 2))
 0
 0
 0
 0))

(: stringnum : String -> (Listof Integer))
;;helper function to convert a string of numbers into a list of numbers
(define (stringnum nums)
  (match (string-split nums)
    ['() '()]
    [(list a)
     (list (string->integer a))]
    [(list a b)
     (cons (string->integer a) (list (string->integer b)))]
    [(list a b c)
     (append (list (string->integer a)) (list (string->integer b))
             (list (string->integer c)))]
    [(list a b c d)
     (append (list (string->integer a)) (list (string->integer b))
             (list (string->integer c)) (list (string->integer d)))]))

(check-expect (stringnum "1 2 3 4") (list 1 2 3 4))
  

(: string->game : String -> Game)
;;deserializes a game
(define (string->game game)
  (local
    {(define g (string-split game "@"))
     (define board (first (string-split game "@")))}
    (Game (string->board board)
          (match (second g)
            ["B" 'Black]
            ["W" 'White])
          (stringnum (third g)))))

(check-expect (string->game
        "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@B@4 6")
              (Game (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0) 'Black (list 4 6)))

(: string->history : String -> (Listof Game))
;;deserializes a history list
(define (string->history games)
  (local
    {(define gs (string-split games "!"))
     (define len (length gs))
     (: loop : Integer -> (Listof Game))
     (define (loop index)
       (if (> len index)
           (append (list (string->game (list-ref gs index)))
                   (loop (add1 index)))
           '()))}
    (loop 0)))

(check-expect
(string->history
(string-append "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0"
"@B@4 5!B2 _ _ _ _ W5 _ W3 _ _ _ B4 W5 _ _ _ B4 _ B5 _ _ _ _ W2|0|0|0|0@B@4"))
(list
 (Game
  (Board
   (list
    (OccupiedPoint 'Black 2)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'White 5)
    'EmptyPoint
    (OccupiedPoint 'White 3)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'Black 5)
    (OccupiedPoint 'White 5)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'Black 3)
    'EmptyPoint
    (OccupiedPoint 'Black 5)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'White 2))
   0
   0
   0
   0)
  'Black
  '(4 5))
 (Game
  (Board
   (list
    (OccupiedPoint 'Black 2)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'White 5)
    'EmptyPoint
    (OccupiedPoint 'White 3)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'Black 4)
    (OccupiedPoint 'White 5)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'Black 4)
    'EmptyPoint
    (OccupiedPoint 'Black 5)
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    'EmptyPoint
    (OccupiedPoint 'White 2))
   0
   0
   0
   0)
  'Black
  '(4))))

;(define-struct World
;  ([game : Game]
;   [style : Style]
;   [bdice : Dice]
;   [wdice : Dice]
;   [click1 : BoardLoc]
;   [highlight : BoardLoc]
;   [history : (Listof Game)]))

(: world->string : World -> String)
;;serializes a world
(define (world->string world)
  (match world
    [(World game style bdice wdice click1 highlight history)
     (history->string (append (list game) history))]))    

(: save-game! : World -> Void)
;;modified save game function given by Professor Wachs on Piazza
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
      (local
        {(: file : Output-Port)
         (define file (open-output-file path))}
           (begin
             (write-string (world->string w) file)
             (close-output-port file)))
      (void))))

(: string->world : Style String -> World)
;;deserializes a world
(define (string->world style w)
  (local
    {(define games (string->history w))
     (define state (first games))}
    (World state style
           (Dice (first (dvalues games)) (second (dvalues games)))
           (Dice (first (dvalues games)) (second (dvalues games)))
           'Nowhere 'Nowhere (rest games))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : Style -> World)
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
      (string->world s (port->string (open-input-file path)))
      (error "load-game: user cancelled"))))
;; === basic style and start board

(define basic-style
  (Style 14
         4
         (draw-checker 'brown)
         (draw-checker 'ivory)
         (draw-point 'saddlebrown)
         (draw-point 'moccasin)
         (draw-background (color 40 10 10) (color 80 20 20))
         (lambda ([s : String]) (text s 12 'black))
         draw-black-die
         draw-white-die))

(define start-board
  (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0))

(test)
