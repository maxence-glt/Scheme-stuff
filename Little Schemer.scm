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
      (member? a (cdr lat)))))))





; rember
(define (rember a lat)
    (cond ((null? lat) nil)
          ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat))))))





; firsts
(define (firsts l)
    (if (null? l)
        nil
        (cons (car (car l)) (firsts (cdr l)))))





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
    (cond ((null? lat) nil)
          (else (if (eq? (car lat) old)
                    (cons new (cons old (cdr lat)))
                    (cons (car lat) (insertL new old (cdr lat)))))))





; subst
(define (subst new old lat)
    (cond ((null? lat) nil)
          (else (if (eq? (car lat) old)
                    (cons new (cdr lat))
                    (cons (car lat) (subst new old (cdr lat)))))))





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
  (cond ((null? lat) nil)
        ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
        ((cons (car lat) (multiinsertR new old (cdr lat))))))





; multiinsertL
(define (multiinsertL new old lat)
  (cond ((null? lat) nil)
        ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
        ((cons (car lat) (multiinsertL new old (cdr lat))))))





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

(define ('+ a b)
  (if (zero? b) a
      ('+ (add1 a) (sub1 b))))

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





; rember* and member*, insertR* and insertL*, occur*, subst*
(define (rember* a l)
  (cond ((null? l) nil)
        ((atom? (car l))
                (if (eq? a (car l)) (rember* a (cdr l))
                    (cons (car l) (rember* a (cdr l)))))
        (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l))
         (if (eq? (car l) a)
             #t
             (member* a (cdr l))))
        (else (or (member* a (car l)) (member* a (cdr l))))))

(define (insertR* new old l)
  (cond ((null? l) nil)
        ((atom? (car l))
         (if (eq? (car l) old)
             (cons old (cons new (insertR* new old (cdr l))))
             (cons (car l) (insertR* new old (cdr l)))))
        (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(define (insertL* new old l)
  (cond ((null? l) nil)
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new (cons (car l) (insertL* new old (cdr l))))
             (cons (car l) (insertL* new old (cdr l)))))
        (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

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





; leftmost
(define (leftmost l)
  (if (atom? (car l))
      (car l)
      (leftmost (car l))))





; eqlist and numbered?
(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (atom? (car l1)) (null? l2)) #f) 
        ((and (atom? (car l1)) (atom? (car l2)))
         (if (eq? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2))
             #f))
        (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))

(define (numbered? aexp)
  (if (atom? aexp)
      (number? aexp)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))





; 2 different value funcs, first for (x + y) second for (+ x y)
(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
        ((eq? (car (cdr nexp)) 'x) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
        (else (expt (value (car nexp)) (value (car (cdr (cdr nexp))))))))

(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        ((eq? (operator nexp) 'x) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        (else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))





; () math
(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zup1 n)
  (cdr n))

(define (new-add a b)
  (if (sero? b)
      a
      (edd1 (new-add a (zup1 b)))))





; set? makeset v1 and makeset v2
(define (set? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (set? (cdr lat)))))

(define (makeset lat) ; FIRST VERSION
  (cond ((null? lat) nil)
        ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
        (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset lat) ; SECOND VERSION
  (if (null? lat)
      nil
      (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))





; subset?
(define (subset? set1 set2)
  (cond ((null? set1) #t)
        ((member? (car set1) set2) (subset? (cdr set1) set2))
        (else #f)))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect set1 set2)
  (cond ((null? set1) nil)
        ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))





; union 
(define (intersect set1 set2)
  (cond ((null? set1) nil)
        ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))





; a-pair?
(define (a-pair? x)
  (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cdr (cdr))) #t)
        (else #f)))





; fun?
(define (fun? rel)
  (set? (firsts rel)))





; revrel
(define (first p) (car p))
(define (second p) (car (cdr p)))
(define (third p) (car (cdr (cdr p))))
(define (build s1 s2) (cons s1 (cons s2 nil)))

(define (revrel rel)
  (cond ((null? rel) nil)
        (else (cons
               (build (first (car rel)) (second (car rel)))
               (revrel (cdr rel))))))





; rember-f
(define (rember-f test? a lat)
    (cond ((null? lat) nil)
          ((test? (car lat) a) (cdr lat))
          (else (cons (car lat) (rember-f test? a (cdr lat))))))





; currying
(define (eq?-c a)
    (lambda (x)
      (eq? x a)))

(define (rember-f test?)
  (lambda (a lat)
    (cond ((null? lat) nil)
          ((test? (car lat) a) (cdr lat))
          (else (cons (car lat) (rember-f test? a (cdr lat)))))))

(define (insert-g seq)
  (lambda (new old l)
    (cond ((null? l) nil)
          (else (if (eq? (car l) old)
                    (seq new old (cdr l))
                    (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
(define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))
(define subst (insert-g (lambda (new old l) (cons new l))))
(define seqrem (lambda (a l) ((insert-g (lambda (new old l) l)) #f a l)))

(define (atom-to-function x)
  (cond ((eq? x '+) add)
        ((eq? x 'x) mul)
        (else expt)))

(define (value nexp)
  ((atom-to-function (car nexp)) (1st-sub-exp nexp) (2nd-sub-exp nexp)))

(define (eq?-c a) (lambda (x) (eq? a x)))





; multirember-f 
(define (multirember-f test?) (lambda (a lat)
  (cond ((null? lat) nil)
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        ((cons (car lat) ((multirember-f test?) a (cdr lat)))))))

(define (multiremberT test? lat)
  (cond ((null? lat) nil)
        ((test? (car lat)) (multiremberT test? (cdr lat)))
        (else (cons (car lat) (multiremberT test? (cdr lat))))))





; friend
(define (a-friend x y) (null? y))

(define col a-friend)

(define (new-friend newlat seen)
  (a-friend newlat (cons 'tuna seen)))





; multirember&co
(define (multirember&co a lat col)
    (cond
     ((null? lat)
      (col nil nil))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen))))))





; multiinsertLR
(define (multiinsertLR new oldL oldR lat)
  (cond ((null? lat) nil)
        ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
        ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
        ((cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))
