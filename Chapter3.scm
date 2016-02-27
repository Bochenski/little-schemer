(load "Chapter2.scm")

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a
                (cdr lat)))))))

(define a 'mint)
(define lat '(lamb chops and mint jelly))
(rember a lat) ;(lamb chops and jelly)

(define a 'mint)
(define lat '(lamb chops and mint flavoured mint jelly))
(rember a lat) ;(lamb chops and flavoured mint jelly)

(define a 'toast)
(define lat '(bacon lettuce and tomato))
(rember a lat) ;(bacon lettuce and tomato)

(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))
(rember a lat) ;(coffee tea cup and hick cup)

(define a 'bacon)
(define lat '(bacon lettuce and tomato))
(rember a lat) ;(lettuce and tomato)

(define a 'and)
(define lat '(bacon lettuce and tomato))
(rember a lat) ;(bacon lettuce tomato)

(define a 'sauce)
(define lat '(soy sauce and tomato sauce))
(rember a lat) ; (soy and tomato sauce)

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define l '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant)))
(firsts l) ;(apple plum grape bean)

(define l '((a b) (c d) (e f)))
(firsts l) ; (a c e)

(define l '())
(firsts l) ; ()

(define l '((five plums) (four) (eleven green oranges)))
(firsts l) ;(five four eleven)

(define l '(((five plums) four) (eleven green oranges) ((no) more)))
(firsts l) ;((five plums) eleven (no))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define new 'topping)
(define old 'fudge)
(define lat '(ice cream with fudge for desert))
(insertR new old lat) ;(ice cream with fudge topping for desert)

(define new 'jalapeno)
(define old 'and)
(define lat '(tacos tamales and salsa))
(insertR new old lat) ;(tacos tamales and jalapeno salsa)

(define new 'e)
(define old 'd)
(define lat '(a b c d f g d h))
(insertR new old lat) ;(a b c d e f g d h)

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL new old lat); (a b c e d f g d h)

(define new 'topping)
(define old 'fudge)
(define lat '(ice cream with fudge for dessert))
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst new old lat); (ice cream with topping for desert)

(define new 'vanilla)
(define o1 'chocolate)
(define o2 'banana)
(define lat '(banana ice cream with chocolate topping))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 new o1 o2 lat); (vanilla ice cream with chocolage topping)

(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember a lat) ;(coffe tea and hick)

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define new 'jalapeno)
(define old 'and)
(define lat '(tacos and tamales and salsa))
(multiinsertR new old lat) ; (tacos and jalapeno tamales and jalapeno salsa)

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define new 'fried)
(define old 'fish)
(define lat '(chips and fish or fish and fried))
(multiinsertL new old lat); (chips and fried fish or fried fish and fried)

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst new old lat); (chips and fried or fried and fried)
