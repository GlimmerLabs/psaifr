#lang racket
(require psaifr/main)

; NOTE: I should rewrite this whole thing as an association list.

(define negative-one '-1)
(define zero '0)
(define one '1)
(define x 'x)
(define y 'y)
(define negate-x '(negate x))
(define sign-x '(sign x))
(define sign-y '(sign y))
(define sign-negate-x '(sign (negate x)))
(define BASIC 
  (list 'negative-one 'zero 'one 
        'x 'y
        'negate-x
        'sign-x 'sign-y
        'sign-negate-x))

(define x-times-half '(* x 0.5))
(define x-times-quarter '(* x 0.5))
(define x-times-y '(* x y))
(define negate-x-times-y '(negate (* x y)))
(define x-squared '(* x x))
(define sign-x-times-y '(* (sign x) y))
(define sign-x-times-sign-y '(* (sign x) (sign y)))
(define MULTIPLICATION 
  (list 'x-times-half 
        'x-times-quarter 
        'x-times-y 
        'negate-x-times-y
        'x-squared))
 
(define x-plus-half '(add x 0.5))
(define x-plus-one '(add x 1))
(define x-minus-half '(add x -0.5))
(define x-plus-y '(add x y))
(define x-minus-y '(add x (negate y)))
(define ADDITION 
  (list 'x-plus-half 
        'x-plus-one
        'x-minus-half 
        'x-plus-y 
        'x-minus-y))

(define x-rotate-one '(rotate x 1))
(define x-rotate-half '(rotate x 0.5))
(define sign-x-rotate-half '(sign (rotate x 0.5)))
(define x-rotate-y '(rotate x y))
(define ROTATION 
  (list 'x-rotate-one 
        'x-rotate-half 
        'sign-x-rotate-half
        'x-rotate-y))

(define x-times-half-plus-y-times-half '(add (* x 0.5) (* y 0.5)))
(define sign-x-plus-y '(sign (add x y)))
(define sign-of-x-times-y '(sign (* x y)))
(define max-x-y '(max x y))
(define COMPLEX 
  (list 'x-times-half-plus-y-times-half 
        'sign-x-plus-y 
        'sign-x-times-y
        'max-x-y))

(define circle1 '(add (* x x) (* y y)))
(define circle2 '(sign (add (* x x) (* y y))))
(define circle3 '(sign (add -0.99 (add (* x x) (* y y)))))
(define CIRCLES 
  (list 'circle1 'circle2 'circle3))

(define sine-x '(sine x))
(define cosine-y '(cosine y))
(define TRIG 
  (list 'sine-x 
        'cosine-y))

(define descriptions
  (append BASIC MULTIPLICATION ADDITION ROTATION COMPLEX CIRCLES TRIG))

(define description->fname
  (lambda (description)
    (symbol->string description)))

(define description->code
  (lambda (description)
    (eval description)))

(define make-small
  (lambda (description)
    (let ((fname (string-append "/tmp/" 
                                (description->fname description) 
                                "-SMALL.png")))
      (display "Creating ")
      (display fname)
      (display " . . . ")
      (flush-output)
      (psaifr-greyscale-small (description->code description) fname)
      (display "Done.")
      (newline))))

(define make-large
  (lambda (description)
    (let ((fname (string-append "/tmp/" 
                                (description->fname description) 
                                "-LARGE.png")))
      (display "Creating ")
      (display fname)
      (display " . . . ")
      (flush-output)
      (psaifr-greyscale-large (description->code description) fname)
      (display "Done.")
      (newline))))

(define make-all-small
  (lambda ()
    (for-each make-small descriptions)))

(define make-all-large
  (lambda ()
    (for-each make-large descriptions)))

(define make-both
  (lambda (description)
    (make-small description)
    (make-large description)))
