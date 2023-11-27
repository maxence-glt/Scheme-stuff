; My self study of the Little Schemer by Daniel P. Friedman and Matthias Felleise ; https://vpb.smallyu.net/%5BType%5D%20books/The%20Little%20Schemer.pdf

; atom? checks if input is an atom (Numbers, Strings, Symbols, Booleans, Characters)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))





; lat? looks at each S-expression in a list, in turn, and asks if each S-expression is an atom, until it runs out of S-expressions 
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))





; member? 
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
      (member? a (cdrlat)))))))





; rember
(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))





; firsts
(define firsts
  (lambda (l)
    (cond 
      ((null l) (quote()))
      (cons (car (car l)) (firsts (cdr l))))))





; insertR
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
        (cond
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        ((else (cons (car lat)
                (insertR new old 
                  (cdr lat))))))))))





; insertL
(define (insertL new old lat)
    (cond
      ((null? lat) (quote()))
      ((cond
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          ((cons (car lat)
                (insertL new old 
                  (cdr lat))))))))





; subst
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons new (cdr lat))))
        ((else (cons (car lat)
                  (subst new old (cdr lat)))))))))





; subst2
(define (subst2 new o1 o2 lat)
  (cond ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
        (cons (car lat) (subst new o1 o2 (cdr lat)))))





; multirember
(define (multirember a lat)
  (cond ((null? lat) (quote()))
        ((eq? (car lat) a) (multirember a (cdr lat)))
        ((cons (car lat) (multirember a (cdr lat))))))





; multiinsertR
(define (multiinsertR new old lat)
  (cond ((null? lat) (quote()))
        ((eq? (old) (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
        ((cons (car lat) (multiinsertR new old (cdr lat))))))





; multiinsertL
(define (multiinsertR new old lat)
  (cond ((null? lat) (quote()))
        ((eq? (old) (car lat)) (cons new (cons old (multiinsertR new old (cdr lat)))))
        ((cons (car lat) (multiinsertR new old (cdr lat))))))





; multisubst
(define multisubst (new old lat)
  (cond ((null? lat) (quote()))
        ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
        (cons (car lat) (multisubst new old cdr lat))))





; + and -
(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (add a b)
  (if (zero? b) a
      (add (add1 a) (sub1 b))))

; using symbol
(define ('- a b)
  (cond ((> b a) #f)
        ((zero? b) a)
        ('- (sub1 a) (sub1 b))))





; addtup (doesn't work)
(define (addtup tup)
  (cond ((null? tup) 0)
        ((+ (car tup) (addtup (cdr tup))))))





; mul and div
(define (mul a b)
  (display b) (newline)
  (if (zero? b) 0
      (add a (mul a (sub1 b)))))

(define (div a b)
  (if (< n m) 0
        (else (add1 (div (- a b) b)))))





; tup+
(define (tup+ tup1 tup2)
  (cond ((and (null? tup1) (null? tup2)) nil)
        ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (add
                     (car tup1) (car tup2))
                    (tup+
                     (cdr tup1) (cdr tup2))))))





; > and <
(define (greater-than a b)
  (cond ((zero? a) #f)
        ((zero? b) #t)
        (else (greater-than (sub1 a) (sub1 b)))))

(define (less-than a b)
  (cond ((zero? b) #f)
        ((zero? a) #t)
        (else (less-than (sub1 a) (sub1 b)))))





; exponent
(define (expt a b)
  (if (zero? b) 1
      (mul a (expt a (sub1 b)))))





; length, pick and rempick
(define (length lst)
  (if (null? lst) 0
      (add1 (length (cdr lst)))))


(define (pick n lst)
  (cond ((null? lst) #f)
        ((eq? 1 n) (car lst))
        ((pick (sub1 n) (cdr lst)))))

(define (rempick n lst)
  (cond ((zero? (sub1 n)) (cdr lst))
        ((cons (car lst) (rempick (sub1 n) (cdr lst))))))





; no-nums and all-nums
(define (no-nums lst)
  (cond ((null? lst) nil)
        ((number? (car lst)) (no-nums (cdr lst)))
        (else (cons (car lst) (no-nums (cdr lst))))))

(define (all-nums lst)
  (cond ((null? lst) nil)
        ((not (number? (car lst))) (all-nums (cdr lst)))
        (else (cons (car lst) (all-nums (cdr lst))))))





; occur
(define (occur a lst)
  (cond ((null? lst) 0)
        ((eq? a (car lst)) (add 1 (occur a (cdr lst))))
        (else (occur a (cdr lst)))))





; rember*, insert*, occur*, subst*
(define (rember* a l)
  (cond ((null? l) nil)
        ((atom? (car l))
                (if (eq? a (car l)) (rember* a (cdr l))
                    (cons (car l) (rember* a (cdr l)))))
        (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond ((null? l) nil)
        ((atom? (car l))
         (if (eq? (car l) old)
             (cons old (cons new (insertR* new old (cdr l))))
             (cons (car l) (insertR* new old (cdr l)))))
        (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (if (eq? (car l) a)
             (add1 (occur* a (cdr l)))
             (occur* a (cdr l))))
        (else (add (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond ((null? l) nil)
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new (subst* new old (cdr l)))
             (cons (car l) (subst* new old (cdr l)))))
        (else (cons (subst* new old (car l)) (subst* new old (cdr l))))))
