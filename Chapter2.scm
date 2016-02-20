(load "Chapter1.scm")

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))

(define l `(Jack Sprat could eat no chicken fat))
(lat? l) ;#t each item of the list is an atom

(define l `((Jack) Sprat could eat no chicken fat))
(lat? l) ;#f the first item is a list

(define l `(Jack (Sprat could) eat no chicken fat))
(lat? l) ;#f the second item is a list

(define l `())
(lat? l) ;#t because it does not contain a list

(define l `(bacon and eggs))
(lat? l) ;#t because it is a list of atoms

(define l '(bacon (and eggs)))
(lat? l) ;#f because (and eggs) is a list

(define l1 '())
(define l2 '(d e f g))
(or (null? l1) (atom? l2)) ;#t because l1 is null

(define l1 '(a b c))
(define l2 '())
(or (null? l1) (atom? l2)) ;#f because l1 is a lat and l2 is not an atom

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define a 'tea)
(define lat '(coffee tea or milk))
(member? a lat) ;#t since tea is in the list

(define a 'poached)
(define lat '(fried eggs and scrambled eggs))
(member? a lat) ;#f since poached is not in the list

(define a 'meat)
(define lat '(mashed potatoes and meat gravy))
(member? a lat) ;#t since meat is in the list

(define a 'liver)
(define lat '(baxels and lox))
(member? a lat) ;#f since liver is not in the list
