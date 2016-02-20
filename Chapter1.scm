; Chapter 1

'atom ;is an atom

'turkey ;is an atom

'1492 ;is an atom

'u ;is an atom

'*abc$ ;is an atom as it is a string of chars without parentheses

'(atom) ;is a list

'(atom turkey or) ;is a list

'(atom turkey) 'or ;not a list - two separate s-expressions, a list and an atom

'((atom turkey) or) ;is a list - as enclosed by parentheses

'xyz ;is an S-expression (all atoms are)a

'(xyz) ;is an S-expression (all lists are)

'((x y) z) ;is an S-expression (it is a list)

'(how are you doing so far) ;is a list with 6 S-expressions (the 6 atoms)

'(((how) are) ((you) (doing so)) far) ; is a list with 3 S-expressions

'() ; is a list, not an atom

'(() () () ()) ; is a list

(define l '(a b c))
(car l) ; a

(define l '((a b c) x y z))
(car l) ; (a b c)

(define l 'hotdog)
;(car l) throws an excption - cannot call car on an atom

(define l '())
;(car l) throws an excption - cannot call car on null list

(define l '(((hotdogs)) (and) (pickle) relish))
(car l) ; ((hotdogs))

(define l '(((hotdogs)) (and)))
(car (car l)); (hotdogs)

(define l '(a b c))
(cdr l) ; (b c)

(define l '((a b c ) x y z))
(cdr l) ; (x y z)

(define l '(hotdog))
(cdr l); ()

(define l '((x) t r))
(cdr l); (t r)

(define a 'hotdogs)
;(cdr a) ;not defined, you cannot call cdr on an atom

(define l '())
;(cdr l) ;not defined, you cannot call cdr on the null list

(define l '((b) (x y) ((c))))
(car (cdr l)) ;(x y)
(cdr (cdr l)) ; (((c)))

(define l '(a (b (c)) d))
;(cdr (car l)) ; not defined as you can't call cdr on an atom (which is car a)

(define a 'peanut)
(define l '(butter and jelly))
(cons a l) ;(peanut butter and jelly)

(define s '(banana and))
(define l '(peanut butter and jelly))
(cons s l) ; ((banana and) peanut butter and jelly)

(define s '((help) this))
(define l '(is very ((hard) to learn)))
(cons s l) ; (((help) this) is very ((hard) to learn))

(define s '(a b (c)))
(define l '())
(cons s l) ; ((a b (c)))

(define s 'a)
(define l '())
(cons s l) ;(a)

(define s '((abc)))
(define l 'b)
;(cons s l) ;not defined as b is not a list

(define s 'a)
(define l 'b)
;(cons s l) ; not defined as b is still not a list

(define s 'a)
(define l '((b) c d))
(cons s (car l)) ;(a b)
(cons s (cdr l)) ;(a c d)

(define l '())
(null? l) ;#t

(null? '()) ;#t

(define l '(a b c))
(null? l) ;#f as it's not an empty list

(define a 'spaghetti)
(null? a) ;#f (strictly null? is undefined for atoms, but it works in real life)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define s `Harry)
(atom? s) ;#t

(define s `(Harry had a heap of apples))
(atom? s)

(define l s)

(atom? (car l)) ;#t
(atom? (cdr l)) ;#f

(define l `(Harry))
(atom? (cdr l)) ;#f

(define l `(swing low sweet cherry oat))
(atom? (car (cdr l))) ;#t

(define l `(swing (low sweet) cherry oat))
(atom? (car (cdr l))) ;#f


;Equality
(define a1 `Harry)
(define a2 `Harry)
(eq? a1 a2) ;#t

(define a1 `margarine)
(define a2 `butter)
(eq? a1 a2) ;#f

;eq should only work on non-numeric atoms
(define l1 `())
(define l2 `(strawberry))
(eq? l1 l2) ;#f

;in practice it works for object equality on lists
(define l1 `(strawberry))
(eq? l1 l2) ;#f
(eq? l1 l1) ;#t
(eq? l2 l2) ;#t

(define n1 6)
(define n2 7)
(eq? n1 n2) ;#f (theoretically eq is not defined for numbers)

(define l `(Mary had a little lamb chop))
(define a `Mary)
(eq? (car l) a) ;#t

(define l `(soured milk))
(define a `milk)
(eq? (cdr l) a) ;#f

(define l `(beans beans we need jelly beans))
(eq? (car l) (car (cdr l))) ;#t
