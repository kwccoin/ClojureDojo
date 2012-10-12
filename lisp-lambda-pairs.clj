; Lisp - lambda in pairs.
; Cons is just the 'functional environment' for a pair.
; Cons contains the values of the pair, named x and y, and the 'call' of a function, named m.
; It contains a call (m x y), thus it is the call for a generic function that operates on a pair. This environment is returned as a function itself.
; Car contains a function, the usual car function that
; yields the first element of a pair; this function is the lambda inside the body.
; Car need an environment to 'run' so its parameter is cons. In the body of car the environment received as parameter is provided with the function needed to fill it and than is executed.
; This call, inside the body of car return the desired result: x.
; Gotcha moment.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons x y))
;=> x

(car (cons x y))
; car  = (z (lambda (p q) p))
; cons = (lambda (m) (m x y))

((cons x y) (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
; m       = (lambda (p q) p)
; (m x y) = x
