;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment09_Adach_Kacper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
(require lang/posn)

; Problem 1

(check-expect (add1* (cons 1 (cons 2 '()))) (cons 2 (cons 3 '())))
(check-expect (plus5 (cons 1 (cons 2 '()))) (cons 6 (cons 7 '())))

(define (plusx n l)
  (cond [(empty? l) '()]
        [else (cons (+ (first l) n)
               (plusx n (rest l)))]))

(define (add1* l)
  (plusx 1 l))

(define (plus5 l)
  (plusx 5 l))

(define (subtract2 l)
  (plusx -2 l))

(check-expect (subtract2 (cons 1 (cons 2 '()))) (cons -1 (cons 0 '())))

; Problem 2

; [Maybe String]

; interpretation: if String is not a String then the statement returns false,
;                 if String is a String then it returns String

; [Maybe [List-of String]]

; interpretation: if List-of String is not a List-of String then the statements returns false,
;                 if List-of String is a List-of String then it returns the List-of String

; [List-of [Maybe String]]

; interpretation: this returns a List-of false values if String is not a String,
;                 this returns a List-of String is String is a String


; the signature means that the function is given a String and a List-of String and returns false or a List-of String

(define (occurs s los)
  (cond [(empty? los) #f]
        [(string=? (first los) s) (rest los)]
        [else (occurs s (rest los))]))

(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)

; Problem 3

(define (convert-euro lous)
  (map (lambda (x) (* x 1.22)) lous))

(check-expect (convert-euro '(1 2 3)) '(1.22 2.44 3.66))

(define (convertFC lof)
  (map (lambda (x) (* (/ 5 9) (- x 32))) lof))

(check-expect (convertFC '(32)) '(0))

(define (translate lop)
  (map (lambda (x) (list (posn-x x) (posn-y x))) lop))

(check-expect (translate (cons (make-posn 1 2) (cons (make-posn 3 4) '()))) (list (list 1 2) (list 3 4)))

; Problem 4

(define-struct ir [name price])
; An IR is 
;   (make-ir String Number)


(define (eliminate-exp ua loir)
  (cond [(empty? loir) '()]
        [else (if (< (ir-price (first loir)) ua) (cons (first loir) (eliminate-exp ua (rest loir))) (eliminate-exp ua (rest loir)))]))

(check-expect (eliminate-exp 8 (cons (make-ir "test" 1) (cons (make-ir "test2" 9) '()))) (list (make-ir "test" 1)))
 
(define (recall ty loir)
  (filter (lambda (x) (not (string=? ty (ir-name x)))) loir))

(check-expect (recall "test" (cons (make-ir "test" 1) (cons (make-ir "nottest" 6) '()))) (list (make-ir "nottest" 6)))

(define (string-in-list list str)
  (cond [(empty? list) #f]
        [else (or (string=? (first list) str) (string-in-list (rest list) str))]))

(define (common-elements list1 list2)
  (cond [(empty? list1) '()]
        [else (if (string-in-list list2 (first list1)) (cons (first list1) (common-elements (rest list1) list2)) (common-elements (rest list1) list2))])) 

(define (selection lon1 lon2)
  (common-elements lon1 lon2))

(check-expect (selection '("test" "test2" "test3") '("test" "test3" "test4")) (list "test" "test3"))

; Problem 5

; [List-of [Number -> Number]]

; Interpretation: this function is given a list-of functions that go from number to number and returns the list of results

(define (at-0 lof)
  (map (lambda (x) (x 0)) lof))

(define (test-func z)
  (+ z 15))

(define (test-func2 z)
  (- z 10))

(check-expect (at-0 (cons test-func (cons test-func2 '()))) (list 15 -10))

; Problem 6

(define (find-string list str)
  (cond [(empty? list) #f]
        [else (or (string=? (first list) str) (string-in-list (rest list) str))]))

(check-expect (find-string '("test" "test2" "test3") "test3") #true)

(define (generic-find-string comp list str)
  (cond [(empty? list) #f]
        [else (or (comp (first list) str) (string-in-list (rest list) str))]))

(define (find-string-case-sensitive list str)
  (generic-find-string string=? list str))

(check-expect (find-string-case-sensitive '("test" "test2" "test3") "test3") #true)

(define (find-string-case-insensitive list str)
  (generic-find-string string-ci=? list str))

(check-expect (find-string-case-insensitive '("test" "test2" "TEST") "test") #true)









