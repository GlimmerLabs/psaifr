#lang racket
(require psaifr/main)

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
(define MULTIPLICATION 
  (list 'x-times-half 
        'x-times-quarter 
        'x-times-y 
        'negate-x-times-y
        'x-squared))

(define sign-x-times-half '(sign (* x 0.5)))
(define sign-x-times-y '(* (sign x) y))
(define sign-x-times-sign-y '(* (sign x) (sign y)))
(define half-sign-x '(* 1/2 (sign x)))
(define SIGN 
  (list 'sign-x-times-half
        'sign-x-times-y
        'sign-x-times-sign-y
        'half-sign-x))
 
(define x-plus-half '(add x 0.5))
(define x-plus-one '(add x 1))
(define x-minus-half '(add x -0.5))
(define x-plus-y '(add x y))
(define x-minus-y '(add x (negate y)))
(define sign-x-plus-half '(sign (+ x 0.5)))
(define sign-x-plus-quarter '(sign (+ x 0.25)))
(define sign-x-minus-half '(sign (+ x -0.5)))
(define sign-x-plus-sign-y '(+ (sign x) (sign y)))
(define ADDITION 
  (list 'x-plus-half 
        'x-plus-one
        'x-minus-half 
        'x-plus-y 
        'x-minus-y
        'sign-x-plus-half
        'sign-x-plus-quarter
        'sign-x-minus-half
        'sign-x-plus-sign-y))

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
(define half-circle `(* ,sign-x-plus-y ,circle1))
(define CIRCLES 
  (list 'circle1 'circle2 'circle3
        'half-circle))

(define vertical-line-1 '(sign (- (abs x) .05)))
(define vertical-line-2 '(sign (- (abs (+ x 1/4)) .1)))
(define diagonal-line-1 '(sign (- (abs (- y x)) .2)))
(define LINES
  (list 'vertical-line-1 'vertical-line-2
        'diagonal-line-1))

(define sine-x '(sine x))
(define cosine-y '(cosine y))
(define TRIG 
  (list 'sine-x 
        'cosine-y))

(define descriptions
  (append BASIC MULTIPLICATION SIGN ADDITION ROTATION 
          COMPLEX CIRCLES LINES TRIG))

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
