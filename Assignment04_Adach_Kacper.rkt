#lang racket
(require 2htdp/image)
 (require 2htdp/universe)

;;(define (lon-temp alon)
;;  (cond [(empty? alon) ...]
;;        [(cons? alon) ... (first alon)
;;                      ... (lon-temp (rest-alon))]))


;; Problem 1

(define-struct lecture-hall (number capacity))
(define-struct automobile (year make model))
(define-struct football-player (name position number))
(define-struct shirt (material size color))

;;
;; lecture-hall:
;; +------------------+-----------------------+
;; |make-lecture-hall | lecture-hall-number   |
;; |                  | lecture-hall-capacity |
;; +------------------+-----------------------+
;;
;; automobile:
;; +----------------+-----------------+
;; |make-automobile | automobile-year |
;; |                | automobile-make |
;; |                | automobile-model|
;; +----------------+-----------------+
;;
;; football-player:
;; +---------------------+-------------------------+
;; |make-football-player | football-player-name    |
;; |                     | football-player-position|
;; |                     | football-player-number  |
;; +---------------------+-------------------------+
;;
;; shirt:
;; +-----------+----------------+
;; |make-shirt | shirt-material |
;; |           | shirt-size     |
;; |           | shirt-color    |
;; +-----------+----------------+

(define example-lecture-hall (make-lecture-hall 201 50))
(define example-automobile (make-automobile 2015 "Ford" "Raptor"))
(define example-football-player (make-football-player "Tom Brady" "QB" 12))
(define example-shirt (make-shirt "cotton" "large" "black"))

#|
(define (lecture-hall-temp alecture-hall)
  (... (lecture-hall-number alecture-hall)
       (lecture-hall-capacity alecture-hall)...))

(define (automobile-temp anautomobile)
  (... (automobile-year anautomobile)
       (automobile-make anautomobile)
       (automobile-model anautomobile)...))

(define (football-player-temp afootball-player)
  (... (football-player-name afootball-player)
       (football-player-position afootball-player)
       (football-player-number afootball-player)...))

(define (shirt-temp ashirt)
  (... (shirt-material ashirt)
       (shirt-size ashirt)
       (shirt-color ashirt)...))
|#

;; Problem 2

(define-struct time (hours minutes))
; A Time is a structure:
;    (make-time Number Number)
; interpretation: (make-time h m) is the time  
; expressed in hours, and minutes
; Constraints:
; – hours is always between 0 and 11
; – minutes is always between 0 and 59


(define (tock given-time)
  (cond [(< (time-minutes given-time) 59) (make-time (time-hours given-time) (+ (time-minutes given-time) 1))]
        [(< (time-hours given-time) 11) (make-time (+ (time-hours given-time) 1) 0)]
        [else (make-time 0 0)]))


(define (time->text time)
  (define time-string (string-append (number->string (time-hours time)) ":" (number->string (time-minutes time))))
  (text time-string 24 "indigo"))

(define START-TIME (make-time 11 59))
(define main
  (big-bang START-TIME [to-draw time->text] [on-tick tock 1]))

;; Problem 3

(define-struct position [x y])
(define-struct movement [speed direction])
(define-struct ball [position movement])

(define (get-direction ball) (movement-direction (ball-movement ball)))
(define (get-new-position-up ball) (make-ball (make-position (position-x (ball-position ball)) (- (position-y (ball-position ball)) (movement-speed (ball-movement ball)))) (ball-movement ball)))
(define (get-new-position-down ball) (make-ball (make-position (position-x (ball-position ball)) (+ (position-y (ball-position ball)) (movement-speed (ball-movement ball)))) (ball-movement ball)))
(define (get-new-position-left ball) (make-ball (make-position (- (position-x (ball-position ball)) (movement-speed (ball-movement ball))) (position-y (ball-position ball))) (ball-movement ball)))
(define (get-new-position-right ball) (make-ball (make-position (+ (position-x (ball-position ball)) (movement-speed (ball-movement ball))) (position-y (ball-position ball))) (ball-movement ball)))

(define TEST (make-position 100 200))
(define TEST2 (make-movement 10 "up"))
(define BALL (make-ball TEST TEST2))

(define (ball-next ball)
  (cond [(string=? (get-direction ball) "up") (get-new-position-up ball)]
        [(string=? (get-direction ball) "down") (get-new-position-down ball)]
        [(string=? (get-direction ball) "left") (get-new-position-left ball)]
        [(string=? (get-direction ball) "right") (get-new-position-right ball)]
        [else ball]))

(define BALL-IMAGE (circle 5 "solid" "red"))
(define EMPTY-SCENE (empty-scene 300 300))

(define (ball-image ball)
  (place-image BALL-IMAGE (position-x (ball-position ball)) (position-y (ball-position ball)) EMPTY-SCENE))

(define (change-direction ball d)
  (make-ball (ball-position ball) (make-movement (movement-speed (ball-movement ball)) d)))
 
(define (ball-change ball key-event)
  (cond [(key=? key-event "up") (change-direction ball "up")]
        [(key=? key-event "down") (change-direction ball "down")]
        [(key=? key-event "left") (change-direction ball "left")]
        [(key=? key-event "right") (change-direction ball "right")]
        [else ball]))

(define main2
  (big-bang BALL [to-draw ball-image] [on-tick ball-next] [on-key ball-change]))