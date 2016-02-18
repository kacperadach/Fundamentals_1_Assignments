#lang racket

(require test-engine/racket-tests)

;; Problem 1

;; Item is one of:
;;     - letter
;;     - box

(define-struct letter (address weight))
(define-struct box (height width length weight))

(define-struct item (mail))

(define box1 (make-box 5 6 7 50))
(define box2 (make-box 20 1000 50 30))
(define letter1 (make-letter "natick" 3.4))
(define item1 (make-item box1))
(define item2 (make-item box2))
(define item3 (make-item letter1))


(define (item-ok? aItem)
  (cond [(letter? (item-mail aItem)) (< (letter-weight (item-mail aItem)) 3.5)]
        [(box? (item-mail aItem)) (and (<= (+ (box-height (item-mail aItem)) (box-width (item-mail aItem)) (box-length (item-mail aItem))) 62)
                                       (<= (* (box-height (item-mail aItem)) (box-width (item-mail aItem)) (box-length (item-mail aItem))) 7938)
                                       (<= (box-weight (item-mail aItem)) 50))]))

(check-expect (item-ok? item1) #t)
(check-expect (item-ok? item2) #f)
(check-expect (item-ok? item3) #t)

;; An LOI is one of:
;;     - empty
;;     - (LOI first rest)

(define-struct LOI (first rest))

(define LOI1 (make-LOI item1 (make-LOI item2 (make-LOI item3 '()))))

(define (bad-items aLOI)
  (cond [(empty? aLOI) '()]
        [(LOI? aLOI) (if (item-ok? (LOI-first aLOI)) (bad-items (LOI-rest aLOI)) (cons (LOI-first aLOI) (bad-items (LOI-rest aLOI))))]))

;(check-expect (bad-items LOI1) '(#<item>))

(define (total-postage aLOI)
  (cond [(empty? aLOI) 0]
        [(LOI? aLOI) (cond [(box? (item-mail (LOI-first aLOI))) (+ (* .15 (box-weight (item-mail (LOI-first aLOI)))) (total-postage (LOI-rest aLOI)))]
                           [(letter? (item-mail (LOI-first aLOI))) (+ .50 (total-postage (LOI-rest aLOI)))])]))

(check-expect (total-postage LOI1) 12.5)