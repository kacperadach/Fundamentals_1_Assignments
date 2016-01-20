#lang racket
(require 2htdp/image)

;; Problem 1
(define tuition 44620)
(define fees 910)
(define room_and_board 14472)
(define (total_cost) (+ tuition fees room_and_board))
(total_cost)
(define (cost_per_lecture) (/ (total_cost) (* (* 4 2) 3 13)))
(cost_per_lecture)

;; Problem 2
#|

Blood Sugar | Insulin Units
____________|______________
100         | 0
110         | 1
120         | 1
130         | 2
140         | 3
150         | 3
160         | 4
170         | 5
180         | 5
190         | 6
200         | 7

|#

(define (insulin_injections blood_sugar)
  (define bs (- blood_sugar 110))
  (cond [(< bs 0) 0] [else (+ 1 (floor (/ bs 15)))]))
(insulin_injections 290)

;; Problem 3
(define (distance_traveled sec) (+ 13 (* 20.75 (sqr sec))))
(distance_traveled 6)

;; Problem 4
(define (pay_per_week containers) (+ 10 (* 4 containers)))

;; Problem 5
(define (greeting name) (string-append "Dear " name ":"))

;; Problem 6
(define (my-star color) (star 12 "solid" color))
(my-star "red")

