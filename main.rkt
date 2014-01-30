#lang racket

(require gigls/unsafe
         racket/date)

(provide (all-defined-out))

; File:
;   psaifr/main.rkt
; Author:
;   Samuel A. Rebelsky
; Summary:
;   Some code for dealing with the PSAIF (Problem Solving, Abstract
;   Images, and Functions) model in Racket.

; HACK!  (Temporary)
(define image-copy-paste-block!
  (lambda params
    params))

; +-------+-----------------------------------------------------------
; | Notes |
; +-------+

; We deal with a bunch of functions that return values in the range 
;    [-1.0 .. 1.0].
; Even colors are in that range.


; +---------+---------------------------------------------------------
; | Helpers |
; +---------+

;;; Procedure:
;;;   cap
;;; Parameters:
;;;   val, a real number
;;; Purpose:
;;;   Restrict val to the range [-1.0 .. 1.0]
;;; Produces:
;;;   restricted, a real number in the range [-1.0 .. 1.0]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is in the range [-1.0 .. 1.0]
;;;     restricted is val
;;;   If val < -1.0
;;;     restricted is -1.0
;;;   If val > 1.0
;;;     restricted is 1.0
(define cap
  (lambda (val)
    (max -1.0 (min 1.0 val))))

;;; Procedure:
;;;   wrap
;;; Parameters:
;;;   val, a real number
;;; Purpose:
;;;   Shift val into the range [-1.0 .. 1.0]
;;; Produces:
;;;   new, a real number in the range [-1.0 .. 1.0]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   Conversion is made according to the following chart
;;;     val:  -5 -4 -3 -2 -1  0  1  2  3  4  5 
;;;        ... |--|--|--|--|--|--|--|--|--|--| ...
;;;     new:  -1  0  1    -1  0  1    -1  0  1
;;;                 -1  0  1    -1  0  1
;;;   with negative odd numbers becoming -1 and positive odd numbers
;;;   becoming positive 1.
(define wrap
  (lambda (val)
    (cond
      ((< val -1.0)
       (wrap (+ val 2.0)))
      ((> val 1.0)
       (wrap (- val 2.0)))
      (else
       val))))

;;; Procedure:
;;;   ndigits
;;; Parameters:
;;;   i, an integer
;;;   n, an integer
;;; Purpose:
;;;   Represent i as a string with at n.
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   0 <= i < 10^n
;;; Postconditions:
;;;   (string-length str) is n
;;;   (string->number str) is i
(define ndigits
  (lambda (i n)
    (let ((tmp (number->string i)))
      (string-append (make-string (- n (string-length tmp)) #\0) tmp))))

;;; Procedure:
;;;   range2component
;;; Parameters:
;;;   r, a value in the range [-1.0 .. 1.0].
;;; Purpose:
;;;   Convert that value to something in the range 0 .. 255
;;; Produces:
;;;   component, an integer in the range [0..255].
(define range2component
  (lambda (r)
    (+ 127.5 (* r 127.5))))

;;; Procedure:
;;;   component2range
;;; Parameters:
;;;   component, a value in the range [0..255]
;;; Purpose:
;;;   Convert to the range [1.0 .. 1.0]
;;; Produces:
;;;   val
(define component2range
  (lambda (c)
    (- (/ c 127.5) 1.0)))

;;; Procedure:
;;;   psaifr-greyscale-image
;;; Parameters:
;;;   fun, a function from two values (x, y) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   width, a positive integer
;;;   height, a positive integer
;;;   border, a non-negative integer
;;; Purpose:
;;;   Generate a new greyscale image of size 
;;;   (width+2*border)+(height+2*border) by applying the functions 
;;;   at each position.
;;; Produces:
;;;   img, an image.
(define psaifr-greyscale-image
  (let ([border-color (irgb 255 255 255)]
        [boundary-color (irgb 128 128 128)])
    (lambda (fun width height border)
      (let ([scale-x (/ 2.0 width)]
            [scale-y (/ 2.0 height)])
        (image-compute (lambda (x y)
                         (cond
                           [(or (< x border)
                                 (< y border)
                                 (> x (+ border width))
                                 (> y (+ border height)))
                             border-color]
                           [(or (= x border) 
                                (= y border)
                                (= x (+ border width))
                                (= y (+ border width)))
                            boundary-color]
                           [else
                             (let* ([newx (- (* (- x border) scale-x) 1.0)]
                                    [newy (- (* (- y border) scale-y) 1.0)]
                                    [result (fun newx newy)]
                                    [component (range2component result)])
                               (irgb component component component))]))
                       (+ width (* 2 border))
                       (+ height (* 2 border)))))))

;;; Procedure:
;;;   psaifr-greyscale-illustration
;;; Parameters:
;;;   fun, an s-expression representing the function
;;;   size, a positive integer
;;;   fontsize, a positive integer
;;; Purpose:
;;;   Generate an illustration of the given function
;;; Produces:
;;;   illustration, an image
(define psaifr-greyscale-illustration
  (lambda (fun size fontsize)
    (let* ([offset (+ fontsize fontsize)]
           [border (+ offset fontsize)]
           [offset2 (- (+ border border size) offset)]
           [middle (+ border (/ size 2))]
           [illustration (psaifr-greyscale-image (makefun fun) 
                                                 size size 
                                                 border)])
      (context-set-fgcolor! "black")
      (context-set-font-size! fontsize)
      (image-display-text! illustration "-1"  
                           offset
                           middle
                           ALIGN-RIGHT
                           ALIGN-CENTER)
      (image-display-text! illustration "-1"  
                           middle
                           offset
                           ALIGN-CENTER
                           ALIGN-BOTTOM)
      (image-display-text! illustration "1"  
                           offset2
                           middle
                           ALIGN-LEFT
                           ALIGN-CENTER)
      (image-display-text! illustration "1"  
                           middle
                           offset2
                           ALIGN-CENTER
                           ALIGN-TOP)
      illustration)))

;;; Procedure:
;;;   psaifr-grayscale-small
;;;   psaifr-grayscale-large
;;; Parameters:
;;;   fun, an s-expression
;;;   fname, the name of a file
;;; Purpose:
;;;   Create a small/large illustration of the given function in the
;;;   PSAIF model.
;;; Produces:
;;;   fname, the same file name
;;; Preconditions:
;;;   The user can write to fname.
;;;   fname names a standard image file format (.jpg, .png, etc.)
;;; Postconditions:
;;;   The file is created.  Whee.
(define psaifr-greyscale-small
  (lambda (fun fname)
    (image-save (psaifr-greyscale-illustration fun 100 10) fname)))

(define psaifr-greyscale-large
  (lambda (fun fname)
    (image-save (psaifr-greyscale-illustration fun 256 24) fname)))

;;; Procedures:
;;;   psaifr-greyscale-unary-small
;;; Parameters:
;;;   trans, a symbol giving a function name
;;;   fun, an s expression giving an image
;;; Purpose:
;;;   Create a short illustration of a transformation
;;; Produces:
;;;   illustration, an image id
(define psaifr-greyscale-unary-small
  (lambda (trans fun)
    (context-set-font-name! "Monospace")
    (context-set-font-size! 12)
    (let* ((width 300)
           (height 200)
           (text (value->string trans))
           (tw (text-width text))
           (illustration (image-new width height))
           (source (psaifr-greyscale-illustration fun 60 7))
           (result (psaifr-greyscale-illustration (list trans fun) 60 7))
           (w (image-width source))
           (h (image-height source)))
      (context-set-font-name! "Monospace")
      (context-set-font-size! 12)
      (image-display-text! illustration 
                           (value->string trans)
                           25
                           (/ height 2)
                           ALIGN-LEFT
                           ALIGN-CENTER)
      (image-copy-paste-block! source 0 0 
                               illustration 60 (/ (- height h) 2)
                               w h)
      (image-display-text! illustration 
                           "="
                           175
                           (/ height 2)
                           ALIGN-LEFT
                           ALIGN-CENTER)
      (image-copy-paste-block! result 0 0 
                               illustration 200 (/ (- height h) 2)
                               w h)

      illustration)))

(define psaifr-greyscale-unary-large
  (lambda (trans fun)
    (context-set-font-name! "Monospace")
    (context-set-font-size! 32)
    (let* ((width 800)
           (height 400)
           (text (value->string trans))
           (tw (text-width text))
           (illustration (image-new width height))
           (source (psaifr-greyscale-illustration fun 200 15))
           (result (psaifr-greyscale-illustration (list trans fun) 200 15))
           (w (image-width source))
           (h (image-height source)))
      (context-set-font-name! "Monospace")
      (context-set-font-size! 32)
      (image-copy-paste-block! source 0 0 
                               illustration 140 (/ (- height h) 2)
                               w h)
      (image-copy-paste-block! result 0 0 
                               illustration 520 (/ (- height h) 2)
                               w h)
      (image-display-text! illustration 
                           (value->string trans)
                           40
                           (/ height 2)
                           ALIGN-LEFT
                           ALIGN-CENTER)
      (image-display-text! illustration 
                           "="
                           450
                           (/ height 2)
                           ALIGN-LEFT
                           ALIGN-CENTER)
      illustration)))

;;; Procedures:
;;;   psaifr-greyscale-triptych-large
;;; Parameters:
;;;   left, an s expression that gives an image
;;;   note1, a string
;;;   middle, an s expression that gives an image
;;;   note2, a string
;;;   right, an s expression that gives an image
;;; Purpose:
;;;   Creates a kind of tryptych, with the parts separate by lnote and rnote
;;; Philosophy:
;;;   Intended to support showing binary functions and other such issues
;;; Plans:
;;;   Should be designed to take into account the size of the generated images,
;;;   rather than my quick hacks to get things working.
(define psaifr-greyscale-triptych-large
  (lambda (left note1 middle note2 right)
    (let* ((width 850)     ; The width of the image we're creating
           (height 400)    ; The height of the image we're creating
           (size 200)      ; The size of the component images
           (fontsize 12)   ; The font size of the image we're creating
           (illustration (image-new width height))
           (leftimg (psaifr-greyscale-illustration left size fontsize))
           (midimg (psaifr-greyscale-illustration middle size fontsize))
           (rightimg (psaifr-greyscale-illustration right size fontsize))
           (w (image-width leftimg))
           (h (image-height leftimg)))
      (context-set-font-name! "Monospace")
      (context-set-font-size! 32)
      (image-copy-paste-block! leftimg 0 0 
                               illustration 0 (/ (- height h) 2)
                               w h)
      (image-copy-paste-block! midimg 0 0 
                               illustration (/ (- width w) 2) (/ (- height h) 2)
                               w h)
      (image-copy-paste-block! rightimg 0 0 
                               illustration (- width w) (/ (- height h) 2)
                               w h)
      (image-display-text! illustration 
                           note1
                           (* width 1/3)
                           (* height 15/32)
                           ALIGN-CENTER
                           ALIGN-CENTER)
      (image-display-text! illustration 
                           note2
                           (* width 2/3)
                           (* height 15/32)
                           ALIGN-CENTER
                           ALIGN-CENTER)
      illustration)))
;;; Procedures:
;;;   psaifr-greyscale-binary-large
;;; Parameters:
;;;   trans, a symbol giving a function name
;;;   left, an s expression giving an image
;;;   right, an s expression giving an image
;;; Purpose:
;;;   Create a short illustration of a transformation
;;; Produces:
;;;   illustration, an image id
;;; Plans:
(define psaifr-greyscale-binary-large
  (lambda (trans left right . rest)
    (let* ((fun (if (null? rest) (list trans left right) (car rest))))
      (psaifr-greyscale-triptych-large left (symbol->string trans) right "=" fun))))

(define unary-illustration-small
  (lambda (fname . stuff)
    (image-save (apply psaifr-greyscale-unary-small stuff) fname)))

(define unary-illustration-large
  (lambda (fname . stuff)
    (image-save (apply psaifr-greyscale-unary-large stuff) fname)))

(define binary-illustration-large
  (lambda (fname . stuff)
    (image-save (apply psaifr-greyscale-binary-large stuff) fname)))

;;; Procedure:
;;;   psaifr-color-image
;;; Parameters:
;;;   rfun, a function from two values (x, y) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   gfun, a function from three values (x, y) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   bfun, a function from three values (x, y) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   width, a positive integer
;;;   height, a positive integer
;;;   border, a non-negative integer
;;; Purpose:
;;;   Generate a new image of size (width+2*border)+(height+2*border)
;;;   by applying the functions at each position.
;;; Produces:
;;;   img, an image.
;(define psaifr-color-image
;  (let ([white (rgb-new 255 255 255)])
;    (lambda (rfun gfun bfun width height border)
;      (let ((half-width (/ width 2.0))
;            (half-height (/ height 2.0)))
;      (image-compute (lambda (x y)
;                       (if (or (< x border)
;                               (< y border)
;                               (> x (border + width))
;                               (> y (border + height)))
;
;                       (let ((newx (- (/ x half-width) 1.0))
;                             (newy (- (/ y half-height) 1.0)))
;                         (rgb-new (range2component (rfun newx newy))
;                                  (range2component (gfun newx newy))
;                                  (range2component (bfun newx newy)))))
;                     width height))))

;;; Procedure: 
;;;   makefun
;;; Parameters:
;;;   body, an s expression
;;; Purpose:
;;;   Given the body of a component function, returns the component function.
;;; Produces:
;;;   fun, a function
(define makefun
  (lambda (body)
    (eval `(lambda (x y) ,body))))

; +------+------------------------------------------------------------
; | Time |
; +------+

;;; Name:
;;;   second
;;; Type:
;;;   real number in the range [-1 .. 1].
;;; Value:
;;;   How far we are in the current minute, scaled to
;;;   the appropriate range
(define second 0)

;;; Name:
;;;   minute
;;; Type:
;;;   real number in the range [-1 .. 1].
;;; Value:
;;;   How far we are in the current hour, scaled to the
;;;   appropriate range
(define minute 0)

;;; Name:
;;;   hour
;;; Type:
;;;   real number in the range [-1 .. 1].
;;; Value:
;;;   How far we are in the current day, scaled to the 
;;;   appropriate range
(define hour 0)

;;; Procedure:
;;;   update-time!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Update the second, minute, and hour variables.
;;; Produces:
;;;   [Nothing; called for the side effect]
(define update-time!
  (lambda ()
    (let ([now (current-date)])
      (set! second (* 1/30 (- (date-second now) 29)))
      (set! minute (* 1/30 (- (date-minute now) 29)))
      (set! hour (* 1/12 (- (date-hour now) 11))))))
(update-time!)

; +------------------+------------------------------------------------
; | Binary Functions |
; +------------------+

; Each of these functions takes two values in the range [-1..1] and
; produce a single value in the range [-1..1].

;;; Procedure:
;;;   add
;;; Parameters:
;;;   x, a real number in the range [-1 .. 1]
;;;   y, a real number in the range [-1 .. 1]
;;; Purpose:
;;;   add x and y, capping the result
;;; Produces:
;;    result, a real number in the range [-1 .. 1]
(define add
  (lambda (a b) (cap (+ a b))))

;;; Procedure:
;;;   rotate
;;; Parameters:
;;;   val, a real number in the range [-1 .. 1]
;;;   amt, a real number in the range [-1 .. 1]
;;; Purpose:
;;;   shift val by amt, wrapping around when we reach 1
;;; Produces:
;;    result, a real number in the range [-1 .. 1]
(define rotate
  (lambda (a b) (wrap (+ a b))))

;;; Procedure:
;;;   *
;;; Parameters:
;;;   x, a real number in the range [-1 .. 1]
;;;   y, a real number in the range [-1 .. 1]
;;; Purpose:
;;;   Multiply x and y
;;; Produces:
;;;    result, a real number in the range [-1 .. 1]

; max
; min

; +-----------------+-------------------------------------------------
; | Unary Functions |
; +-----------------+

(define ave
  (lambda (a b) (/ (+ a b) 2)))

(define sine
  (o sin (l-s * pi)))

(define cosine
  (o cos (l-s * pi)))

(define accuracy 0.001)
(define sign
  (lambda (val)
    (cond
      [(< (abs val) accuracy)
       0]
      [(< val 0) -1]
      [else 1])))

(define negate -)

; +-------------------+-----------------------------------------------
; | Zeroary Functions |
; +-------------------+

; second - current second (in the range -1.0 to 1.0)
; minute - current minute (in the range -1.0 to 1.0)
; hour - current hour (in the range -1.0 to 1.0)
; x - x value
; y - y value
; mouseX - x position of mouse [unimplemented]
; mouseY - y position of mouse [unimplemented]
; clickX - x position of last click [unimplemented]
; clickY - y position of last click [unimplemented]
; # - the given number (any number in the range [-1 .. 1]

; Experiments
(define e0
  (lambda ()
    (psaifr-greyscale-image (makefun 'x) 255 255 20)))
