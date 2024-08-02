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





; 2.18 (idk if helper functions aren't in the spirit of answering SICP problems!)
(define (remove-last-pair l)
  (if (null? (cdr l))
      nil
      (cons (car l) (remove-last-pair (cdr l)))))

(define (reverse l)
  (if (null? l)
      nil
      (cons (last-pair l) (reverse (remove-last-pair l)))))





; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define (first-denomination l)
  (car l))

(define (no-more? l)
  (null? l))

(define (except-first-denomination l)
  (cdr l))

; It doesn't matter as to what order the coins list is since we're using tree recursion
; and the tree will just be in a different order but with the same final result





; 2.20 *GO BACK TO THIS ONE (DRRacket has a bug I think where it passes a double list into rest)
(define (same-parity first . rest)
  (cond ((null? rest) nil)
        ((even? (- (car rest) first))
         (cons (car rest) (same-parity first (cdr rest))))
        (else (same-parity first (cdr rest)))))





; 2.21
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))





; 2.22
; its reversed since answer starts with nil then 1 then 4 then 9 and so on
; and since we are consing answer to the back of the car, its reversed

; this doesn't work since we're consing lists together 





; 2.23
(define (for-each proc list)
  (map proc list))





; 2.24
(1 (2 (3 4)))





; 2.25
(define list1 '(1 3 (5 7) 9))
(define list2 '((7)))
(define list3 '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list1)))))
(car (car list2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))





; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)    ; (1 2 3 4 5 6)
(cons x y)      ; ((1 2 3) 4 5 6)
(list x y)      ; ((1 2 3) (4 5 6))





; 2.27
(define (deep-reverse l)
    (cond ((null? l) nil)
          ((not (pair? l)) l)
          (else (cons (deep-reverse (last-pair l))
                      (deep-reverse (remove-last-pair l))))))





; 2.28 *GO BACK TO THIS ONE (not sure about using append)
(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? l)) (cons l nil))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))





; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight ( branch-structure(right-branch mobile))))))

(define (is-balanced? mobile)
  (if (or (not (pair? mobile)) (null? mobile))
      #t
      (and (eq? (* (total-weight (branch-structure (left-branch mobile)))
                   (branch-length (left-branch mobile)))
                (* (total-weight (branch-structure (right-branch mobile)))
                   (branch-length (right-branch mobile))))
           (and (is-balanced? (branch-structure (left-branch mobile)))
                (is-balanced? (branch-structure (right-branch mobile)))))))

; you'd need to change the selector functions





; 2.30
(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-direct (car tree))
                    (square-tree-direct (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))





; 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))





; 2.32 HARD but I suck at permutaions
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))




; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))





; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))





; 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree t))))





; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))





; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define v1 (list (list 1 -1 2) (list 0 -3 1)))
(define v2 (list 2 1 0))
(matrix-*-vector v1 v2)

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))





; 2.38
; 1 1/2
; 1/6
; (1 (2 (3 ())))
; (((() 1) 2) 3)

; op must be associative because if not the order of operations matters
; addition and multiplication work but division and subtraction dont





; 2.39 (substitute accumulate for fold-right)
(define (reverse1 sequence)
  (accumulate (lambda (x y) (append y (cons x nil))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))





; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n)))





; 2.41
(define (sum-equal? tuple n)
  (= n (accumulate + 0 tuple)))

(define (enumerate-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (ordered-triples n s)
  (filter (lambda (x) (sum-equal? x s)) (enumerate-triples n)))





; i'm too lazy to do 2.42 and 2.43
; 2.44
