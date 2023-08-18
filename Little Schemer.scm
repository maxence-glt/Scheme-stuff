; My self study of the Little Schemer by Daniel P. Friedman and Matthias Felleise
; https://vpb.smallyu.net/%5BType%5D%20books/The%20Little%20Schemer.pdf

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