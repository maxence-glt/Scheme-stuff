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
        ((eq? (car lat) (old)) (cdr lat))
        ((else (cons (car lat)
                (insertR new old (cdr lat))))))))))