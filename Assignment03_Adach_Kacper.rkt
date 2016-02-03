#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require lang/posn)

;; Problem 1

; A NinjaTurtle is one of four turtles:
; - "Leonardo"
; - "Michaelangelo"
; - "Raphael"
; - "Donatello"
; interpretation each name is a NinjaTurtle

(define-struct NinjaTurtle [turtle])

#|
(define is-ninja-turtle (name)
  (cond
    [(string=? "Leonardo" name) ...]
    [(string=? "Michaelangelo" name) ...]
    [(string=? "Raphael" name) ...]
    [(string=? "Donatello" name) ...]))
|#

(check-expect (percentage 20) #true)
(check-expect (percentage 110) #false)

(define (percentage number)
  (if (or (< number 0) (> number 100)) #false #true))
   
;; Problem 2

#|
// diagram
  <-----------------\       
[0] --> [1] --> [2] |
|#

(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)

(define (tl-next-numeric current-state)
  (modulo (+ current-state 1) 3))

(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")

(define (tl-next current-state)
  (cond
    [(string=? "red" current-state) "green"]
    [(string=? "green" current-state) "yellow"]
    [(string=? "yellow" current-state) "red"]))

#|
The tl-next convey's its intention more clearly because it returns strings
that have the value of the light rather than arbitrary ints.
|#

;; Problem 3

(define SPEED 3)
(define-struct balld [location direction])
(make-balld 20 "down")

(define-struct vel [deltax deltay])
(define ball1 (make-balld (make-posn 30 40) (make-vel -10 5)))


;; Problem 4

(define SCENE (empty-scene 200 200))
(define BLUE-CIRCLE (circle 5 "solid" "blue"))
(define GREEN-CIRCLE (circle 5 "solid" "green"))
(define RED-CIRCLE (circle 5 "solid" "red"))
(define rposn (make-posn 5 20))
(define bposn (make-posn 100 50))


(define (get-green-x)
  (+ (* 0.9 (abs (- (posn-x bposn) (posn-x rposn)))) (posn-x rposn)))

(define (get-green-y)
  (+ (* 0.9 (abs (- (posn-y bposn) (posn-y rposn)))) (posn-y rposn)))

(define (main red blue)
  (add-line (place-image BLUE-CIRCLE (posn-x blue) (posn-y blue)
               (place-image RED-CIRCLE (posn-x red) (posn-y red)
               (place-image GREEN-CIRCLE (get-green-x) (get-green-y) SCENE)))
               (posn-x rposn) (posn-y rposn) (posn-x bposn) (posn-y bposn) "black"))

(main rposn bposn)
  


 
