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





; 1.16
(define (iter-fast-expt b n a)
  (cond ((= n 0) (* a 1))
        ((even? n) (iter-fast-expt b (/ n 2) (square a)))
        (else (iter-fast-expt b (- n 1) (* a b)))))

(iter-fast-expt 3 3 1) ; 27





; 1.17
(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (half b))))
        (else (+ a (mul a (- b 1))))))





; 1.18
(define (iter-mul a b n)
  (display (list a b n)) (newline)
  (cond ((= b 0) n)
        ((even? b) (iter-mul (double a) (half b) n))
        (else (iter-mul a (- b 1) (+ n a)))))

(define (new-mul a b) (iter-mul a b 0))





; skipping 19 (too much to read) and 20





; 1.21 
; 199: 199 (prime), 1999: 1999 (prime), 19999: 7





; 1.22
(define (search-for-primes lower upper amount)
  (if (not (even? lower))
      (cond ((= amount 0) (display "done"))
            ((> lower upper) (display "done"))
            ((prime? lower) (display lower) (timed-prime-test lower) (newline))))
  
  (if (not (or (= amount 0) (> lower upper)))
      (if (prime? lower)
          (search-for-primes (+ lower 1) upper (- amount 1))
          (search-for-primes (+ lower 1) upper amount))))

(search-for-primes 1000 10000 3)              ; 1009: 3 microsecs, 1013: 1, 1019: 2                AVG: 3.2
(search-for-primes 10000 100000 3)            ; 10007: 3, 10009: 4, 10037: 3                            2.0
(search-for-primes 100000 1000000 3)          ; 100003: 10, 100019: 11, 100043: 11                      3.4
(search-for-primes 1000000 10000000 3)        ; 1000003: 32, 1000033: 31, 1000037: 32                   10.7
(search-for-primes 10000000 100000000 3)      ; 10000019: 96, 10000079: 95, 10000103: 94                31.7
(search-for-primes 100000000 1000000000 3)    ; 100000007: 314, 100000037: 295, 100000039: 306          305
(search-for-primes 1000000000 10000000000 3)  ; 1000000007: 982, 1000000009: 951, 1000000021: 974       969

; sqrt(10) is about 3.1622, and you can clearly see that it is increasing at that rate!
; lower numbers take a similar amount of time due to how fast our newew hardware is
; so, increasing the numbers by a lot yields very visible results

; yes, my result IS compatible with that notion!





; 1.23
; The new function looks like this
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor
               n
               ((lambda (x) (if (= x 2) 3 (+ x 2))) test-divisor)))))

; Rerunning the last search-for-primes' results with the new function didn't change anything on my pc





; 1.24
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(search-for-primes 1000 10000 3)              ; 1009: 124, 1013: 126, 1019: 130
(search-for-primes 10000 100000 3)            ; 10007: 153, 10009: 148, 10037: 149
(search-for-primes 100000 1000000 3)          ; 100003: 173, 100019: 176, 100043: 178
(search-for-primes 1000000 10000000 3)        ; 1000003: 194, 1000033: 212, 1000037: 203
(search-for-primes 10000000 100000000 3)      ; 10000019: 235, 10000079: 244, 10000103: 253
(search-for-primes 100000000 1000000000 3)    ; 100000007: 279, 100000037: 303, 100000039: 282
(search-for-primes 1000000000 10000000000 3)  ; 1000000007: 314, 1000000009: 308, 1000000021: 327

; We can see that while initially it takes longer to compute the primes, the growth is logarithmic





; 1.25
; No since you need the remainder to work on the number thats being carried through
; If you add (display x) (newline) in square you'll see the huge difference
; The old (expmod 5 101 101) goes 5 -> 24 -> 71 -> 92 -> 1 -> 1
; Thew new (expmod 5 101 101) goes 5 -> 125 -> 15625 -> 244140625 -> 298023223876953125 -> 88817841970012523233890533447265625





; 1.26
; I think it takes so much more time since Scheme needs to evaluate expmod twice before multiplying it together
; instead of just evaluating it once then multiplying that number by itself
; As for the time ocmplexity change, thats due to how it used to be a "simple" linear recursion which is now a tree recursion!





; 1.27
; Here's my procedure:
(define (new-fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (define (full-fermat-test n i)
    (if (= i n)
        #t
        (if (try-it i) (full-fermat-test n (+ i 1)) #f)))
  (full-fermat-test n 1))

(new-fermat-test 561)   ; #t
(new-fermat-test 1105)  ; #t
(new-fermat-test 1729)  ; #t
(new-fermat-test 2465)  ; #t
(new-fermat-test 2821)  ; #t
(new-fermat-test 6601)  ; #t

; and of course, none of these are actually prime





; 1.28 (will come back to this one)

; 1.29 (will also come back)

; 1.30
(define (simpson-integral f a b n) 
  (define (term x) (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h))))) 
  (define (next x) (+ x (* 2 h))) 
  (define h (/ (- b a) n)) 
  (* (/ h 3) (sum term a next (- b (* 2 h))))) 





; 1.31
