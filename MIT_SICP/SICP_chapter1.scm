; My exersise answers to MIT's Structure and Interpretation of Computer Programs book (by Abelson and Sussman)
; I purchased the book on amazon, but I found a pdf here: https://media.githubusercontent.com/media/sarabander/sicp-pdf/master/sicp.pdf
; Not all exersises require coding so the exerise answers' order will be scattered

; 1.1 - The Elements of Programming
; Exersise 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6

(define a 3) ; a= 3
(define b (+ a 1)) ; b = 4
( + a b (* a b)) ; 19
;(a = b)  #f (errors)

(if (and (> b a) (< b (* a b)))
    b
    a) ; b (4)

(cond ((= a 4) 6) ; #f
      ((= b 4) (+ 6 7 a)) ; #t, 16
      (else 25))

(+ 2 (if (> b a) b a)) ; 6

(* (cond ((> a b) a) ; #f
         ((< a b) b) ; #t, b (4)
         (else -1))
   (+ a 1)) ; (* b (+ a 1)) == 16





; 1.2
(/ 
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
    (* 3 (- 6 2) (- 2 7)))





; 1.3
(define (sum_squares x y z)
  (cond ((and (>= x z) (>= y z)) (+ (* x x) (* y y)))
        ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
        ((and (>= z y) (>= x y)) (+ (* x x) (* z z)))))





; 1.4
; If b is greater than 0, the if will return a + and will add a and b. If not, it will subtract a and b.





; 1.5
; Normal order fully expands and then reduces, while applicative order evaluates arguments and then applies.
; Ben, with a normal order evaluation, will see that since x == 0, it will return 0.
; With applicative order evaluation, it will evaluate 0 and p, and p will evaluate over and over forever.





; 1.6
; In new-if, the third argument will keep evaluating itself due to applicative order





; 1.7
; With really large numbers, the interpreter will evaulate the sqrts to 1 * 10^x
; With really small numbers, the interpreter will evaluare the sqrts to 0.03 and random numbers. Both are wrong
; (Examples) sqrt 9999999999999999 = 100000000.0 | sqrt 0.0000000004 = 0.03125...
; If you add (display guess) (newline) to sqrt-itr you can see how the guess gets very unnacurate with larger x values.





; 1.8
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
       guess
      (cbrt-iter (improve guess x) x)))

(define (good-enough? guess x)
(< (abs (- (cube guess) x)) 0.001))

(define (cube x) (* x x x))

(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(cbrt 27) ; 3
(cbrt 64) ; 4
(cbrt 125) ; 5





; 1.2 - Procedures and the Processes they Generate
; 1.9
; Process 1:
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
; You get 9
; This is a recursive process as the shape of the process is one that expands and then contracts





(define (+ a b)
  if (= a 0) b (+ (dec a) (inc b)))
; You also get 9
; This one is an iterative process as it evaluates a and b before calling the func again





; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10) == 1024
(A 2 4) == 65536
(A 3 3) == 65536

; Don't know how to do maths proofs yet so ill try my best
(define (f n) (A 0 n)) == 2n
(define (g n) (A 1 n)) == n = 0 ;??




; 1.11
; Recursive process
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f(- n 1)) (* 2 (f(- n 2))) (* 3 (f(- n 3)))))))





; Iterative process
(define (f-iter n)
  (define (fn a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (fn (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (fn 2 1 0 (- n 2)))





; 1.12
(define (pascal row column)
  (if ((or (= column 1) (= row column)) 1)
        (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column))))





; 1.14
; Drew out tree on paper and got 4 as final answer
; I think the number of steps is n ^ n
; And the growth of space is the height of the tree of course





; 1.15
; a. 5 times since you need to divide angle 5 times to get it under 0.1
; and each time you do so it calls p again
; b. Order of growth in space = linear