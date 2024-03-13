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
