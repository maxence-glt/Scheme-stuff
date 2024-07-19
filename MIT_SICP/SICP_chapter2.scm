#lang SICP

(define (numer x) (car x))
(define (denom x) (car (cdr x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    
    (cons (/ n g) (cons (/ d g) nil))))

(define frac (make-rat 3 -4))
(print-rat frac)





; 2.2
(define (make-point x y)
  (cons x (cons y nil)))

(define (x-point point)
  (car point))

(define (y-point point)
  (car (cdr point)))

(define (make-segment point1 point2)
  (cons point1 (cons point2 nil)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (car (cdr segment)))

(define (midpoint-segment line)
  (make-segment (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)  (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2))) 





; 2.3
(define (perimeter-rectangle r)
  (+ (* 2 (width-rectangle r)) (* 2 height-rectangle r)))

(define (area-rectangle r)
  (* width-rectangle height-rectangle))

(define (width-rectangle r)
  (+ (abs (left r)) (abs (left r))))

(define (length-rectangle r)
  (+ (abs (bottom r)) (abs (top r))))
; I'm not doing the whole thing since I've done the same thing in 61a and its tedious






; 2.4
(define (cdr z)
  (z (lambda (p q) q)))





; 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (if (= (remainder n 2) 1)
      0
      (+ 1 (car (/ n 2)))))

(define (cdr n)
  (if (= (remainder n 3) 1)
      0
      (+ 1 (car (/ n 3)))))





; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))





; 2.7, 2.8, 2.9, 2.10, 2.11 idk what it meansx, 2.12, 2.13
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (if (> (lower-bound x) (lower-bound y))
                     (- (lower-bound x) (lower-bound y))
                     (- (lower-bound y) (lower-bound x)))
                 (if (> (upper-bound x) (upper-bound y))
                     (- (upper-bound x) (upper-bound y))
                     (- (upper-bound y) (upper-bound x)))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (if (= (upper-bound y) 0)
                      0 ; display error some how
                      (/ 1.0 (upper-bound y)))
                  (if (= (lower-bound y) 0)
                      0 ; display error some how
                      (/ 1.0 (upper-bound y))))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

(define (percent i)
  (* 100 (- (/ (upper-bound i) (center i)) 1)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
  
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define f (make-center-percent 2 5))
(define s (make-center-percent 3 4))

(par1 f f)
(par2 f f)





; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))





; 
