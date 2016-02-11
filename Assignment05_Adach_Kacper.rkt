#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require lang/posn)

;; Problem 1

(define-struct regular (id base years))
(define-struct classic (id price))

(define (yearlyPrice movie)
  (* (regular-base movie) (/ (- 100 (* 3.5 (regular-years movie))) 100)))

(define (getRegPrice movie)
  (if (< (yearlyPrice movie) 2) 2 (yearlyPrice movie)))

(define (getPrice movie)
  (cond [(regular? movie) (getRegPrice movie)]
        [(classic? movie) (classic-price movie)]))

(define r (make-regular "id" 70 2))
(define c (make-classic "id" 50))

(check-expect (yearlyPrice r) 65.1)
(check-expect (getRegPrice r) 65.1)
(check-expect (getPrice c) 50)
(check-expect (getPrice r) 65.1)

;; Problem 2


;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle
 
(define-struct circl [x y r outline c])
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct squar [x y size outline c])
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: x and y determine the origin of the squar, size
;;   is the lenght of one side of the squar, outline whether it's outlined or solid,
;;   and c its color
 
(define-struct recta [x y width height outline c])
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: x and y deteremine the origin of the recta, width and height
;;   are the width of height of the recta's sides, outline whether its outlined or solid,
;;   and c its color

;; template
;; (define (temp-shape ashape)
;;   (cond [(circl? ashape) ... ]
;;         [(squar? ashape) ... ]
;;         [(recta? ashape) ... ]))

;; ... problem solving steps ...

(define (shape-shift-x sh delta)
  (cond [(circl? sh) (make-circl (+ (circl-x sh) delta) (circl-y sh) (circl-r sh) (circl-outline sh) (circl-c sh))]
        [(squar? sh) (make-squar (+ (squar-x sh) delta) (squar-y sh) (squar-size sh) (squar-outline sh) (squar-c sh))]
        [(recta? sh) (make-recta (+ (recta-x sh) delta) (recta-y sh) (recta-width sh) (recta-height sh) (recta-outline sh) (recta-c sh))]))

(define (shape-in? sh p)
  (cond [(circl? sh) (>= (circl-r sh) (sqrt (+ (sqr (- (posn-x p) (circl-x sh))) (sqr (- (posn-y p) (circl-y sh))))))]
        [(squar? sh) (and (>= (posn-x p) (squar-x sh)) (<= (posn-x p) (+ (squar-x sh) (squar-size sh))) (>= (posn-y p) (squar-y sh)) (<= (posn-y p) (+ (squar-y sh) (squar-size sh))))]
        [(recta? sh) (and (>= (posn-x p) (recta-x sh)) (<= (posn-x p) (+ (recta-x sh) (recta-width sh))) (>= (posn-y p) (recta-y sh)) (<= (posn-y p) (+ (recta-y sh) (recta-height sh))))]))

(define (shape-draw sh sc)
  (cond [(circl? sh) (place-image (circle (circl-r sh) (if (circl-outline sh) "solid" "outlined") (circl-c sh)) (circl-x sh) (circl-y sh) sc)]
        [(squar? sh) (place-image (square (squar-size sh) (if (squar-outline sh) "solid" "outlined") (squar-c sh)) (squar-x sh) (squar-y sh) sc)]
        [(recta? sh) (place-image (rectangle (recta-width sh) (recta-height sh) (if (recta-outline sh) "solid" "outlined") (recta-c sh)) (recta-x sh) (recta-y sh) sc)]))
;; inspect for expected results:
(define sh (make-squar 100 100 50 true 'red))
(define pt (make-posn  130 130))
 
(shape-in? sh pt)
(shape-draw (make-circl 130 130 5 true 'red)
            (shape-draw sh
                        (empty-scene 300 300)))



;; Problem 3

(define (checkLength password min max)
  (and (>= (string-length password) min) (< (string-length password) max)))

(define (passwords-6-11? passwordlist)
  (cond [(empty? passwordlist) #t]
        [(string? (car passwordlist)) (and (checkLength (car passwordlist) 6 11) (passwords-6-11? (cdr passwordlist)))]))

(define (passwords-ok? passwordlist min max)
  (cond [(empty? passwordlist) #t]
        [(string? (car passwordlist)) (and (checkLength (car passwordlist) min max) (passwords-6-11? (cdr passwordlist)))]))

(define PASSWORDLIST (cons "123456" (cons "12345" '())))

(check-expect (checkLength "test" 4 18) #true)
(check-expect (passwords-6-11? PASSWORDLIST) #false)
(check-expect (passwords-ok? PASSWORDLIST 3 10) #true)

;; Problem 4

(define-struct ball [x y color])
(define ballone (make-ball 20 100 "red"))
(define balltwo (make-ball 300 30 "blue"))

;; Ball = (make-ball Number Number Color)
;; Color is one of 'red, 'yellow, 'blue, etc.

(define-struct ballList [ball list])
(define test (make-ballList ballone (make-ballList balltwo '())))

;; template
;; (define (temp aballList)
;;    (cond [(empty? (ballList-list aballList)) ... (ballList-ball aballList)]
;;          [(ballList? (ballList-list aballList)) ... (ballList-ball aballList)
;;                                                     (temp (ballList-list aballList))]))

(define (lob-length list)
  (cond [(empty? (ballList-list list)) 1]
        [(ballList? (ballList-list list)) (+ 1 (lob-length (ballList-list list)))]))

(check-expect (lob-length test) 2)

(define (lob-x list)
  (cond [(empty? (ballList-list list)) (cons (ball-x (ballList-ball list)) '())]
        [(ballList? (ballList-list list)) (cons (ball-x (ballList-ball list)) (lob-x (ballList-list list)))]))

(check-expect (lob-x test) '(20 300))

(define EMPTY-SCENE (empty-scene 300 300))
(define RADIUS 3)
(define (draw-ball ball) (circle RADIUS "solid" (ball-color ball)))
  
(define (lob-draw list)
  (cond [(empty? (ballList-list list)) (place-image (draw-ball (ballList-ball list)) (ball-x (ballList-ball list)) (ball-y (ballList-ball list)) EMPTY-SCENE)]
        [(ballList? (ballList-list list)) (place-image (draw-ball (ballList-ball list)) (ball-x (ballList-ball list)) (ball-y (ballList-ball list)) (lob-draw (ballList-list list)))]))

(define (valid-ball ball)
  (and (<= (ball-x ball) 300) (>= (ball-x ball) 0) (<= (ball-y ball) 300) (>= (ball-y ball) 0)))

(check-expect (valid-ball ballone) #true)

(define (lob-filter list)
  (cond [(empty? (ballList-list list)) (if (valid-ball (ballList-ball list)) (cons (ballList-ball list) '()) '())]
        [(ballList? (ballList-list list)) (if (valid-ball (ballList-ball list)) (cons (ballList-ball list) (lob-filter (ballList-list list))) (lob-filter (ballList-list list)))]))

(define (compare-balls ball1 ball2)
  (and (= (ball-x ball1) (ball-x ball2)) (= (ball-y ball1) (ball-y ball2)) (string=? (ball-color ball1) (ball-color ball2))))

(check-expect (compare-balls ballone balltwo) #false)

(define (lob-member? lob b)
  (cond [(empty? (ballList-list lob)) (compare-balls b (ballList-ball lob))]
        [(ballList? (ballList-list lob))  (or (compare-balls b (ballList-ball lob)) (lob-member? (ballList-list lob) b))]))

(check-expect (lob-member? test ballone) #true)

;; Problem 5

(define-struct txt [content x y])
;; Txt = (make-txt String Number Number)
;; Represents the occurrence of the given text at the given location,
;; in computer-graphics coordinates.
 
;; LoTxt is one of:
;; -- empty
;; -- (cons Txt LoTxt)
 
(define-struct world [image hidden])
;; World = (make-world Image LoTxt)
;; intepretation:
;;  The world's image represents the image that the audience can see.
;;  The world's list of Txt represents the yet-to-be-revealed elements.

(define EMPTY (empty-scene 400 400))
(define OYM (make-txt "On your mark." 100 50))
(define GS (make-txt "Get set." 100 150))
(define GO (make-txt "Go!" 100 250))
(define WORDLIST (cons OYM (cons GS (cons GO '()))))
(define INITIAL (make-world EMPTY WORDLIST))

(define (display world) (world-image world))

(define (next world)
  (cond [(empty? (world-hidden world)) (make-world EMPTY WORDLIST)]
        [(pair? (world-hidden world)) (make-world
                                       (place-image (text (txt-content (car (world-hidden world))) 20 "blue") (txt-x (car (world-hidden world))) (txt-y (car (world-hidden world))) (world-image world))
                                       (cdr (world-hidden world)))]))

(define main (big-bang INITIAL (on-tick next 1) (to-draw display)))
