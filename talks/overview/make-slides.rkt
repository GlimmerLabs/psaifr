#lang racket
(require psaifr/main)

; NOTE: I should rewrite this whole thing as an association list.

(define negative-one '(-1 "negative-one"))
(define zero '(0 "zero"))
(define one '(1 "one"))
(define x '(x "x"))
(define y '(y "y"))
(define BASIC (list negative-one zero one x y))

(define x-times-half '((* x 0.5) "x-times-half"))
(define x-times-quarter '((* x 0.5) "x-times-quarter"))
(define x-times-y '((* x y) "x-times-y"))
(define negate-x-times-y '((negate (* x y)) "negate-x-times-y"))
(define x-squared '((* x x) "x-squared"))
(define MULTIPLICATION (list x-times-half x-times-quarter x-times-y x-squared))
  
(define x-plus-half '((add x 0.5) "x-plus-half"))
(define x-plus-one '((add x 1) "x-plus-one"))
(define x-minus-half '((add x -0.5) "x-minus-half"))
(define x-plus-y '((add x y) "x-plus-y"))
(define x-minus-y '((add x (negate y)) "x-minus-y"))
(define ADDITION (list x-plus-half x-minus-half x-plus-y x-minus-y))

(define x-rotate-one '((rotate x 1) "x-rotate-one"))
(define x-rotate-half '((rotate x 0.5) "x-rotate-half"))
(define x-rotate-y '((rotate x y) "x-rotate-y"))
(define ROTATION (list x-rotate-one x-rotate-half x-rotate-y))

(define negate-x '((negate x) "negate-x"))
(define sign-x '((sign x) "sign-x"))
(define sign-y '((sign y) "sign-y"))
(define sign-negate-x '((sign (negate x)) "sign-negate-x"))
(define MISC (list negate-x sign-x sign-y))

(define x-times-half-plus-y-times-half '((add (* x 0.5) (* y 0.5)) "x-times-half-plus-y-times-half"))
(define sign-x-plus-y '((sign (add x y)) "sign-x-plus-y"))
(define sign-x-times-y '((sign (* x y)) "sign-x-times-y"))
(define max-x-y '((max x y) "max-x-y"))
(define COMPLEX (list x-times-half-plus-y-times-half 
                      sign-x-plus-y 
                      sign-x-times-y
                      max-x-y))

(define circle1 '((add (* x x) (* y y)) "circle1"))
(define circle2 '((sign (add (* x x) (* y y))) "circle2"))
(define circle3 '((sign (add -0.99 (add (* x x) (* y y)))) "circle3"))
(define CIRCLES (list circle1 circle2 circle3))

(define sine-x '((sine x) "sine-x"))
(define cosine-y '((cosine y) "cosine-y"))
(define TRIG (list sine-x cosine-y))

(define descriptions
  (append BASIC MULTIPLICATION ADDITION ROTATION MISC COMPLEX CIRCLES TRIG))

(define make-small
  (lambda (description)
    (let ((fname (string-append "/tmp/" (cadr description) "-SMALL.png")))
      (display "Creating ")
      (display fname)
      (display " . . . ")
      (flush-output)
      (psaifr-greyscale-small (car description) fname)
      (display "Done.")
      (newline))))

(define make-large
  (lambda (description)
    (let ((fname (string-append "/tmp/" (cadr description) "-LARGE.png")))
      (display "Creating ")
      (display fname)
      (display " . . . ")
      (flush-output)
      (psaifr-greyscale-large (car description) fname)
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
