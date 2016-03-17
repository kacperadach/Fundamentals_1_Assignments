#lang racket


(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require lang/posn)

(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))
 
(define PLAYER (square CELL-SIZE 'solid 'black))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD (overlay/align "left" "middle" (rotate 90 TONGUE) CENTIPEDE-CELL))
(define RIGHT-HEAD (overlay/align "right" "middle" (rotate 30 TONGUE) CENTIPEDE-CELL))
 
(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'LightSalmon)
(define MUSHROOM-2-C 'Salmon)
(define MUSHROOM-3-C 'OrangeRed)
(define MUSHROOM-4-C 'DarkRed)
 
(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))


;; Game is a make-Game Centipede Player LoM
(define-struct Game (centipede player lom))
;; template
;;(define (template-for-game aGame)
;;  ((Centipede-first (Game-centipede aGame)) (Centipede-rest (Game-centipede aGame)) ... (Game-player aGame) ... (LoM-first (Game-lom aGame)) (LoM-rest (Game-lom aGame))))

;; a Centipede is one of:
;; make-Centipede Cell Centipede
;; '()
(define-struct Centipede (first rest))
;;template
;;(define (template-for-centipede aCentipede)
;;  (cond [(empty? (Centipede-rest aCentipede)) ... ]
;;        [else (Centipede-first aCentipede) ... (Centipede-rest aCentipede) ... (template-for-centipede (Centipede-rest aCentipede))]))

;; a Cell is a make-Cell image Posn String String
(define-struct Cell (image position direction-horizontal direction-vertical))

;; a Player is a make-Player Int Bullet
(define-struct Player (position bullet))

;; a Bullet is a make-Bullet int int
(define-struct Bullet (x y))

;; a Mushroom is a make-Mushroom int int int
(define-struct Mushroom (hp x y))

;; a LoMushroom is a make-LoM Mushroom LoM
(define-struct LoM (first rest))

;; get the y value for the Posn of Cell when moving
(define (get-y cell)
  (cond [(and (equal? (posn-y (Cell-position cell)) 0) (string=? (Cell-direction-vertical cell) "up")) 1]
        [(and (equal? (posn-y (Cell-position cell)) 40) (string=? (Cell-direction-vertical cell) "down")) 39]
         [else (cond [(string=? (Cell-direction-vertical cell) "up") (- (posn-y (Cell-position cell)) 1)]
                     [(string=? (Cell-direction-vertical cell) "down") (+ (posn-y (Cell-position cell)) 1)])]))

;; get the vertical direction for the Cell when moving
(define (get-vertical-direction cell)
  (cond [(and (equal? (posn-y (Cell-position cell)) 0) (string=? (Cell-direction-vertical cell) "up")) "down"]
        [(and (equal? (posn-y (Cell-position cell)) 40) (string=? (Cell-direction-vertical cell) "down")) "up"]
         [else (cond [(string=? (Cell-direction-vertical cell) "up") "up"]
                     [(string=? (Cell-direction-vertical cell) "down") "down"])]))

;; movement function for the centipede
(define (move-centipede-cell cell lom)
  (cond [(string=? (Cell-direction-horizontal cell) "right") (cond [(equal? (posn-x (Cell-position cell)) 25) (make-Cell (switch-image cell) (make-posn 25 (get-y cell)) "left" (get-vertical-direction cell))]
                                                                   [(mushroom-next-cell (+ 1 (posn-x (Cell-position cell))) (posn-y (Cell-position cell)) lom) (make-Cell (Cell-image cell) (make-posn (posn-x (Cell-position cell)) (if (string=? (Cell-direction-vertical cell) "down") (+ (posn-y (Cell-position cell)) 1) (- (posn-y (Cell-position cell)) 1))) "right" (get-vertical-direction cell))]
                                                                   [else (make-Cell (Cell-image cell) (make-posn (+ (posn-x (Cell-position cell)) 1) (posn-y (Cell-position cell))) "right" (Cell-direction-vertical cell))])]
        [(string=? (Cell-direction-horizontal cell) "left") (cond [(equal? (posn-x (Cell-position cell)) 0) (make-Cell (switch-image cell) (make-posn 0 (get-y cell)) "right" (get-vertical-direction cell))]
                                                                  [(mushroom-next-cell (- (posn-x (Cell-position cell)) 1) (posn-y (Cell-position cell)) lom) (make-Cell (Cell-image cell) (make-posn (posn-x (Cell-position cell)) (if (string=? (Cell-direction-vertical cell) "down") (+ (posn-y (Cell-position cell)) 1) (- (posn-y (Cell-position cell)) 1))) "left" (get-vertical-direction cell))]
                                                                  [else (make-Cell (Cell-image cell) (make-posn (- (posn-x (Cell-position cell)) 1) (posn-y (Cell-position cell))) "left" (Cell-direction-vertical cell))])]))

                                                      
                                                     
;; check to see if next-cell is a mushroom
(define (mushroom-next-cell x y lom)
  (cond [(empty? lom) #f]
        [(empty? (LoM-rest lom)) (and (equal? (Mushroom-x (LoM-first lom)) x) (equal? (Mushroom-y (LoM-first lom)) y) (> (Mushroom-hp (LoM-first lom)) 0))]
        [else (or (and (equal? (Mushroom-x (LoM-first lom)) x) (equal? (Mushroom-y (LoM-first lom)) y) (> (Mushroom-hp (LoM-first lom)) 0)) (mushroom-next-cell x y (LoM-rest lom)))]))
         


;; changes image when cell moves 
(define (switch-image cell)
  (cond [(equal? (Cell-image cell) LEFT-HEAD) RIGHT-HEAD]
        [(equal? (Cell-image cell) RIGHT-HEAD) LEFT-HEAD]
        [else CENTIPEDE-CELL]))

;; movement function for centipede
(define (move-centipede centipede lom)
  (cond [(empty? centipede) '()]
        [(empty? (Centipede-rest centipede)) (make-Centipede (move-centipede-cell (Centipede-first centipede) lom) '())]
        [else (make-Centipede (move-centipede-cell (Centipede-first centipede) lom) (move-centipede (Centipede-rest centipede) lom))]))
  
;; drawing function for cell
(define (draw-cell cell background)
  (place-image (Cell-image cell) (* (posn-x (Cell-position cell)) CELL-SIZE) (* (posn-y (Cell-position cell)) CELL-SIZE) background))


;; drawing function for centipede
(define (draw-centipede centipede background)
  (cond [(empty? centipede) (place-image empty-image 0 0 background)]
        [(empty? (Centipede-rest centipede)) (draw-cell (Centipede-first centipede) background)]
        [else (draw-cell (Centipede-first centipede) (draw-centipede (Centipede-rest centipede) background))]))

;; drawing function for mushroom
(define (draw-mushroom mushroom background)
  (cond [(equal? (Mushroom-hp mushroom) 4) (place-image (circle MUSHROOM-RADIUS "solid" MUSHROOM-4-C) (* (Mushroom-x mushroom) CELL-SIZE) (* (Mushroom-y mushroom) CELL-SIZE) background)]
        [(equal? (Mushroom-hp mushroom) 3) (place-image (circle MUSHROOM-RADIUS "solid" MUSHROOM-3-C) (* (Mushroom-x mushroom) CELL-SIZE) (* (Mushroom-y mushroom) CELL-SIZE) background)]
        [(equal? (Mushroom-hp mushroom) 2) (place-image (circle MUSHROOM-RADIUS "solid" MUSHROOM-2-C) (* (Mushroom-x mushroom) CELL-SIZE) (* (Mushroom-y mushroom) CELL-SIZE) background)]
        [(equal? (Mushroom-hp mushroom) 1) (place-image (circle MUSHROOM-RADIUS "solid" MUSHROOM-1-C) (* (Mushroom-x mushroom) CELL-SIZE) (* (Mushroom-y mushroom) CELL-SIZE) background)]
        [else (place-image empty-image 0 0 background)]))

; drawing function for list of mushrooms
(define (draw-lom lom background)
  (cond [(empty? lom) (place-image empty-image 0 0 background)]
        [(empty? (LoM-rest lom)) (draw-mushroom (LoM-first lom) background)]
        [else (draw-mushroom (LoM-first lom) (draw-lom (LoM-rest lom) background))]))

;; drawing function for player
(define (draw-player player background)
  (cond [(empty? (Player-bullet player)) (place-image PLAYER (- (* (Player-position player) CELL-SIZE) (* .5 CELL-SIZE)) (- (* 40 CELL-SIZE) (* .5 CELL-SIZE)) background)]
        [else (place-image PLAYER (- (* (Player-position player) CELL-SIZE) (* .5 CELL-SIZE)) (- (* 40 CELL-SIZE) (* .5 CELL-SIZE))
                           (place-image BULLET (* CELL-SIZE (Bullet-x (Player-bullet player))) (* CELL-SIZE (Bullet-y (Player-bullet player))) background))]))

; drawing function for all things contained by game
(define (draw-all player centipede lom)
  (draw-player player (draw-centipede centipede (draw-lom lom BG))))

;; initializes a centipede of a certain length (used in big-bang)
(define (initialize-centipede length max)
  (cond [(equal? length 1) (make-Centipede (make-Cell (if (equal? max length) RIGHT-HEAD CENTIPEDE-CELL) (make-posn length 0) "right" "down") '())]
        [else (make-Centipede (make-Cell (if (equal? max length) RIGHT-HEAD CENTIPEDE-CELL) (make-posn length 0) "right" "down") (initialize-centipede (- length 1) max))]))

  
;; key-input controlling function for player
(define (control-player player key-event)
  (cond [(key=? key-event "right") (if (equal? (Player-position player) 25) (make-Player 25 (Player-bullet player)) (make-Player (+ (Player-position player) 1) (Player-bullet player)))]
        [(key=? key-event "left") (if (equal? (Player-position player) 1) (make-Player 1 (Player-bullet player)) (make-Player (- (Player-position player) 1) (Player-bullet player)))]
        [(key=? key-event " ") (if (empty? (Player-bullet player)) (make-Player (Player-position player) (make-Bullet (Player-position player) 40)) (make-Player (Player-position player) (Player-bullet player)))]))

;; movement function for bullet
(define (move-bullet player)
  (cond [(empty? (Player-bullet player)) player]
        [else (cond [(equal? (Bullet-y (Player-bullet player)) 0) (make-Player (Player-position player) '())]
                    [else (make-Player (Player-position player) (make-Bullet (Bullet-x (Player-bullet player)) (- (Bullet-y (Player-bullet player)) 1)))])]))

;; function checks if a bullet is in a centipede
(define (bullet-in-centipede centipede player)
  (cond [(empty? (Player-bullet player)) #f]
        [else (cond [(empty? (Centipede-rest centipede)) (bullet-in-cell-helper (Centipede-first centipede) (Player-bullet player))] 
                    [else (or (bullet-in-cell-helper (Centipede-first centipede) (Player-bullet player)) (bullet-in-centipede (Centipede-rest centipede) player))])])) 

;; function is a helper for above function
(define (bullet-in-cell-helper cell bullet)
  (and (equal? (Bullet-x bullet) (posn-x (Cell-position cell))) (equal? (Bullet-y bullet) (posn-y (Cell-position cell)))))

; returns opposite horizontal direction
(define (get-direction-horizontal direction)
  (cond [(string=? "left" direction) "right"]
        [(string=? "right" direction) "left"]))

; helper for shot-centipede, returns a centipede
(define (shot-centipede-helper centipede)
  (cond [(empty? centipede) '()]
        [(empty? (Centipede-rest centipede)) (make-Centipede (make-Cell CENTIPEDE-CELL (Cell-position (Centipede-first centipede)) (get-direction-horizontal (Cell-direction-horizontal (Centipede-first centipede))) (Cell-direction-vertical (Centipede-first centipede))) '())]
        [else (make-Centipede (make-Cell (Cell-image (Centipede-first centipede)) (Cell-position (Centipede-first centipede)) (get-direction-horizontal (Cell-direction-horizontal (Centipede-first centipede))) (Cell-direction-vertical (Centipede-first centipede))) (shot-centipede-helper (Centipede-rest centipede)))]))


;; returns new centipede after it has been shot
(define (shot-centipede centipede bullet)
  (cond [(bullet-in-cell-helper (Centipede-first centipede) bullet) (shot-centipede-helper (Centipede-rest centipede))]
        [else (make-Centipede (Centipede-first centipede) (shot-centipede (Centipede-rest centipede) bullet))]))

; returns whether a bullet has hit a mushroom in a list of mushrooms
(define (bullet-in-lom player lom)
  (cond [(empty? lom) #f]
        [(empty? (Player-bullet player)) #f]
        [(empty? (LoM-rest lom)) (and (equal? (Bullet-x (Player-bullet player)) (Mushroom-x (LoM-first lom))) (equal? (Bullet-y (Player-bullet player)) (Mushroom-y (LoM-first lom))) (> (Mushroom-hp (LoM-first lom)) 0))]
        [else (or (and (equal? (Bullet-x (Player-bullet player)) (Mushroom-x (LoM-first lom))) (equal? (Bullet-y (Player-bullet player)) (Mushroom-y (LoM-first lom))) (> (Mushroom-hp (LoM-first lom)) 0)) (bullet-in-lom player (LoM-rest lom)))]))

; returns whether a bullet has hit a mushroom
(define (bullet-in-mushroom player mushroom)
  (and (equal? (Bullet-x (Player-bullet player)) (Mushroom-x mushroom)) (Bullet-y (Player-bullet player)) (Mushroom-y mushroom)))

; reduces hp of mushroom if shot
(define (shot-mushroom mushroom)
  (make-Mushroom (- (Mushroom-hp mushroom) 1) (Mushroom-x mushroom) (Mushroom-y mushroom)))

; returns a new lom if it has been shot
(define (shot-lom lom player)
  (cond [(empty? lom) '()]
        [(empty? (LoM-rest lom)) (if (bullet-in-mushroom player (LoM-first lom)) (make-LoM (shot-mushroom (LoM-first lom)) '()) (make-LoM (LoM-first lom) '()))]
        [else (if (bullet-in-mushroom player (LoM-first lom)) (make-LoM (shot-mushroom (LoM-first lom)) (shot-lom (LoM-rest lom) player)) (make-LoM (LoM-first lom) (shot-lom (LoM-rest lom) player)))]))

; adds a mushroom to lom where a centipede has been shot
(define (add-new-mushroom lom x y)
  (make-LoM (make-Mushroom 4 x y) lom))

;; returns a new player with an empty bullet object for when a bullet hits
(define (bullet-hit player)
  (make-Player (Player-position player) '()))

;; checks to see if the player has been hit by the centipede
(define (player-hit player centipede)
  (and (equal? (Player-position player) (posn-x (Cell-position (Centipede-first centipede)))) (equal? (posn-y (Cell-position (Centipede-first centipede))) 40)))

;; drawing function for the game                     
(define (draw-game game)
  (draw-all (Game-player game) (Game-centipede game) (Game-lom game)))

;; ticking function for the game
(define (tick-game game)
  (cond [(bullet-in-centipede (Game-centipede game) (Game-player game)) (make-Game (move-centipede (shot-centipede (Game-centipede game) (Player-bullet (Game-player game))) (Game-lom game)) (move-bullet (bullet-hit (Game-player game))) (add-new-mushroom (Game-lom game) (Bullet-x (Player-bullet (Game-player game))) (Bullet-y (Player-bullet (Game-player game)))))]
        [(bullet-in-lom (Game-player game) (Game-lom game)) (make-Game (move-centipede (Game-centipede game) (Game-lom game)) (move-bullet (bullet-hit (Game-player game))) (shot-lom (Game-lom game) (Game-player game)))]
        [else (make-Game (move-centipede (Game-centipede game) (Game-lom game)) (move-bullet (Game-player game)) (Game-lom game))]))

;;player control function for the game
(define (control-game game key-event)
  (make-Game (Game-centipede game) (control-player (Game-player game) key-event) (Game-lom game))) 

;; stop function for the game
(define (stop-game game)
  (or (empty? (Game-centipede game)) (player-hit (Game-player game) (Game-centipede game))))

; returns image to display at end of game
(define (stop-game-scene game)
  (if (empty? (Game-centipede game)) (place-image WINNER 150 100 BG) (place-image LOSER 150 100 BG)))

; constant for list of mushroom
(define INITIAL-LOM (make-LoM (make-Mushroom 4 20 20) (make-LoM (make-Mushroom 4 3 3) (make-LoM (make-Mushroom 4 10 20) (make-LoM (make-Mushroom 4 5 13)  (make-LoM (make-Mushroom 4 18 12)  (make-LoM (make-Mushroom 4 6 32) (make-LoM (make-Mushroom 4 19 36) (make-LoM (make-Mushroom 4 21 15) (make-LoM (make-Mushroom 4 23 34) (make-LoM (make-Mushroom 4 9 36) (make-LoM (make-Mushroom 4 18 4) '()))))))))))))


;; main function with big-bang
(define (main length) (big-bang (make-Game (initialize-centipede length length) (make-Player 20 '()) INITIAL-LOM) [stop-when stop-game stop-game-scene] [to-draw draw-game] [on-tick tick-game] [on-key control-game]))

(main 8)
  