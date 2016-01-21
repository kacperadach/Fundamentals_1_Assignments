#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

;; Problem 1
(define (favorite-star pent-len color sq-len)
  (overlay (star pent-len "solid" color) (square sq-len "solid" "black")))
(favorite-star 30 "red" 50)

;; Problem 2)
(define SCENE-HEIGHT 100)
(define SCENE-WIDTH 500)
(define WHEEL-RADIUS 7) 
(define RECT-HEIGHT (* WHEEL-RADIUS 2))
(define RECT-LENGTH (* WHEEL-RADIUS 8))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define BOTTOM-RECT (rectangle RECT-LENGTH RECT-HEIGHT "solid" "red"))
(define TOP-RECT (rectangle (/ RECT-LENGTH 2) RECT-HEIGHT "solid" "red"))
(define TREE (underlay/xy (circle 10 "solid" "green") 9 15 (rectangle 2 20 "solid" "brown")))
(define BACKGROUND (place-image TREE 100 80 (empty-scene SCENE-WIDTH SCENE-HEIGHT)))

(define (render x) (place-image TOP-RECT (+ x (* 2 WHEEL-RADIUS)) (- SCENE-HEIGHT (* 4 WHEEL-RADIUS))
                     (place-image WHEEL x (- SCENE-HEIGHT WHEEL-RADIUS)
                     (place-image WHEEL (+ x (* 4 WHEEL-RADIUS)) (- SCENE-HEIGHT WHEEL-RADIUS)
                     (place-image BOTTOM-RECT (+ x (* 2 WHEEL-RADIUS)) (- SCENE-HEIGHT (* 2 WHEEL-RADIUS)) BACKGROUND)))))
(check-expect (tock 10) 20)
(define (tock x) (+ x 10))
(check-expect (stop-car 10) false)
(check-expect (stop-car 1000) true)
(define (stop-car x) (>= x (+ SCENE-WIDTH 15)))
(define (main x) (big-bang x [to-draw render] [on-tick tock] [stop-when stop-car]))
(test)
(main 0)

;; Problem 3
(define (tock_problem3 x) (+ x (* 10 (sin x))))
(define (main_problem3 x) (big-bang x [to-draw render] [on-tick tock_problem3]))
(define (hyper x-position-of-car x-mouse y-mouse me) (if (equal? me "button-down") x-mouse x-position-of-car))
(define (main_click x) (big-bang x [to-draw render] [on-tick tock] [on-mouse hyper]))
(main_click 10)

;; Problem 4
(define (human-sensation humidity)
  (cond [(> humidity 65) "it is humid"]
        [(< humidity 20) "it is dry"]
        [else "it is comfortable"]))
(human-sensation 10)
(human-sensation 50)
(human-sensation 90)