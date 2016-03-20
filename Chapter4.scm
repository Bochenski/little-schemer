(load "Chapter1.scm")

(define n 14)
(atom? n); #t

(number? -3); #t -3 is a number, but we do not consider negative numbers
(number? 3.14159); #t is also a number, but we consider only whole numbers

(define add1
  (lambda (n)
    (+ n 1)))

(define n 67)
(add1 n); 68
(add1 67); 68

(define sub1
  (lambda (n)
    (- n 1)))

(define n 5)
(sub1 n); 4

(sub1 0); -1 in practice but we do not define it.

(zero? 0); #t
(zero? 1492); #f

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12); 58

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(o- 14 3); 11
(o- 17 9); 8
(o- 18 25); No answer (no negatives) - in practice -7


(define tup?
  (lambda (lat)
  (cond
    ((null? lat) #t)
    ((number? (car lat)) (tup? (cdr lat)))
    (else #f))))

(tup? '(2 11 3 79 47 6)) ;#t
(tup? '(8 55 5 555)) ;#t
(tup? '(1 2 8 apple 4 3)) ;#f its a lat
(tup? '(3 (7 4) 13 9)) ;#f (7 4) is not a number
(tup? ());#t

(define addtup
  (lambda (tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup) (addtup (cdr tup)))))))
(define tup '(3 5 2 8))
(addtup tup); 18

(define tup '(15 6 7 12 3))
(addtup tup); 43

(define x
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (o+ n (x n (sub1 m)))))))

(x 5 3); 15
(x 13 4); 52
(x 12 3); 36

(define tup1 '(3 6 9 11 4))
(define tup2 '(8 5 2 0 7))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ tup1 tup2); (11 11 11 11 11)

(define tup1 '(2 3))
(define tup2 '(4 6))
(tup+ tup1 tup2); (6 9)

(define tup1 '(3 7))
(define tup2 '(4 6))
(tup+ tup1 tup2); (7 13)

(define tup1 '(3 7))
(define tup2 '(4 6 8 1))
(tup+ tup1 tup2);(7 13 8 1)

(define tup1 '(3 7 8 1))
(define tup2 '(4 6))
(tup+ tup1 tup2); (7 13 8 1);

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 12 133); #f
(> 120 11); #t

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(< 4 6); #t
(< 8 3); #f
(< 6 6); #f

(define =
  (lambda (n m)
    (cond
      ((< n m) #f)
      ((< m n) #f)
      (else #t))))
(= 6 6) ;#t
(= 0 6) ;#f
(= 3 4) ;#f

(define expn
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (expn n (sub1 m)))))))

(expn 1 1); 1
(expn 2 3); 8
(expn 5 3); 125

(define /
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (/ (o- n m) m))))))

(/ 15 4); 3

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define lat '(hotdogs with mustard sauerkraut and pickles))
(len lat); 6
(define lat '(ham and cheese on rye))
(len lat); 5

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define lat '(lasgna spaghetti ravioli macaroni meatball))
(pick 4 lat); macaroni

(define lat '(a))
;(pick 0 lat); not defined

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define lat '(hotdogs with hot mustard))
(rempick 3 lat); (hotdogs with mustard)

(define a 'tomato)
(number? a) ;#f

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define lat '(a 12 b 3 2 c))
(no-nums lat); (a b c)
(all-nums lat); (12 3 2)

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(eqan? 'a 'b); #f
(eqan? 'a 'a); #t
(eqan? 'a 2); #f
(eqan? 3 'b); #f
(eqan? 5 6); #f
(eqan? 99 99); #t

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define a 'and)
(define lat '(burger and chips and beans and ice cream))
(occur a lat); 3

(define one?
  (lambda (n)
    (= n 1)))

(one? 1); #t
(one? 0); #f
;(one? 'bob) not defined

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define n 3)
(define lat '(lemon meringue salty pie))
(rempick n lat); (lemon meringue pie)
