; My self study of the Little Schemer, starting with common functions

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