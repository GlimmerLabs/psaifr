;;; File:
;;;   genetic-pen.scm
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A first stab at writing some genetic algorithms for pen-based drawings
;;;   (which I suppose could be implemented as raster graphics or vector
;;;   graphics or ...)
;;; Overview:
;;;   Notes - Notes on design and implementation
;;;   Globals - Some global settings
;;; Copyright:
;;;   Copyright (c) 2013 Samuel A. Rebelsky.  All rights reserved
;;;
;;;   This program is free software: you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation, either version 3 of the License, or
;;;   (at your option) any later version.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program.  If not, see <http://www.gnu.org/licenses/>.

; +-------+-----------------------------------------------------------
; | Notes |
; +-------+

; * We represent the instructions ("sketch") as a vector of alternating
;   instructions and parameters.

; * We also store a starting location and direction.

; * All of the instructions assume a current location and direction
;   for the pen.

; * Valid instructions:

; * Valid parameters: 
;   * Any real number in the range [0.0,1.0)
;   * 'x, representing the percentage of the way across the screen we are
;   * 'y, representing the percentage of the way down the screen we are
;   * 'n, my favorite parameter

; Hmmm ... should I make every other instruction a block when creating
; a random set of instructions?  It will certainly make loops and
; conditionals easier

; +--------+----------------------------------------------------------
; | Macros |
; +--------+

; Not working in this version of Scheme.  :-(.

; +---------+---------------------------------------------------------
; | Globals |
; +---------+

;;; Variable:
;;;   NUM-INSTRUCTIONS
;;; Description:
;;;   The number of instructions in a sketch
(define NUM-INSTRUCTIONS 20)

;;; Variable:
;;;   MAX-LEN
;;; Description:
;;;   The maximum length a pen will move.  (Hmmm ... effects scaling.)
(define MAX-LEN 100)

;;; Variable:
;;;   MAX-LOOP
;;; Description:
;;;   The maximum number of steps in a loop.
(define MAX-LOOP 10)

;;; The list of valid genetic pen commands
(define SKETCH.COMMANDS
  (vector 'sketch.block
          'sketch.forward 
          'sketch.if
          'sketch.loop 
          'sketch.noop
          'sketch.pen   ; Flip from up to down or back again
          'sketch.size  ; Change the size of the pen
          'sketch.turn 
          ))

; +-----------------+-------------------------------------------------
; | Misc. Utilities |
; +-----------------+

;;; Procedure:
;;;   loop
;;; Parameters:
;;;   n, an integer
;;;   fun, a zero-ary function
;;; Purpose:
;;;   Execute fun n times
;;; Produces:
;;;   Nothing.  Called for the side effect
(define loop
  (lambda (n fun)
    (when (> n 0)
      (fun)
      (loop (- n 1) fun))))

;;; Procedure:
;;;   unit-scale
;;; Parameters:
;;;   u, a real number in the range [0,1)
;;;   max, an integer
;;; Purpose:
;;;   Convert u to an integer in the range [1,x]
;;; Produces:
;;;   i, an integer
(define unit-scale
  (lambda (u max)
    (inexact->exact (+ 1 (floor (* u max))))))

;;; Procedure:
;;;   vector-random-elt
;;; Parameters:
;;;   vec, a vector
;;; Purpose:
;;;   Pick a random element of the vector.
;;; Produces:
;;;   val
(define vector-random-elt
  (lambda (vec)
    (vector-ref vec (random (vector-length vec)))))

(define object-field-getter
  (lambda (num)
    (lambda (object)
      (vector-ref object num))))

(define object-field-setter
  (lambda (num)
    (lambda (object val)
      (vector-set! object num val))))


; +------+------------------------------------------------------------
; | Pens |
; +------+

; Pens store x, y, and angle
(define pen-new
  (lambda params
    (if (null? params)
        (vector 0 0 0 #t)
        (list->vector params))))

(define pen-x (object-field-getter 0))

(define pen-set-x! (object-field-setter 0))

(define pen-y (object-field-getter 1))

(define pen-set-y! (object-field-setter 1))

(define pen-theta (object-field-getter 2))

(define pen-set-theta! (object-field-setter 2))

(define pen-down? (object-field-getter 3))

(define pen-up? (^not pen-down?))

;;; Procedure:
;;;   pen-forward!
;;; Parameters:
;;;   pen, a pen
;;;   image, an image
;;; Purpose:
;;;   Move the pen forward on the image
(define pen-forward!
  (lambda (pen image amt)
    (let* ((x0 (pen-x pen))
           (y0 (pen-y pen))
           (theta (pen-theta pen))
           (x1 (+ x0 (* (cos theta) amt)))
           (y1 (+ y0 (* (sin theta) amt)))
           (w (image-width image))
           (h (image-height image)))
      (when (pen-down? pen)
        (display (list 'x0 x0 'y0 y0 'x1 x1 'y1 y1)) (newline)
        (image-draw-line! image x0 y0 x1 y1))
      ; The new position can be outside the bounds of the image.  Wrap around.
      (when (< x1 0)
        (set! x1 (+ x1 w))
        (set! x0 (+ x0 w)))
      (when (>= x1 w)
        (set! x1 (- x1 w))
        (set! x0 (- x0 w)))
      (when (< y1 0)
        (set! y1 (+ y1 h))
        (set! y0 (+ y0 h)))
      (when (>= y1 h)
        (set! y1 (- y1 h))
        (set! y0 (- y0 h)))
      ; Draw the second line
      (when (pen-down? pen)
        (display (list 'x0 x0 'y0 y0 'x1 x1 'y1 y1)) (newline)
        (image-draw-line! image x0 y0 x1 y1))
      (pen-set-x! pen x1)
      (pen-set-y! pen y1))))

(define pen-turn!
  (lambda (pen image amt)
    (pen-set-theta! pen (+ (pen-theta pen) amt))))

(define pen-noop!
  (lambda (pen image amt)
    void))


; +--------------+------------------------------------------------------
; | Environments |
; +--------------+

; Fields: pen, image, n

(define env-new
  (lambda params
    (if (null? params)
        (vector (pen-new)      ; pen
                (image-show (image-new 100 100)) ; image
                0 ; n
                )
        (list->vector params))))

(define env-pen (object-getter 0))

(define env-image (object-getter 1))

(define env-n (object-getter 2))

;;; Procedure:
;;;   env-param
;;; Parameters:
;;;   env, an environment
;;;   p,  information on the parameter (it may be a number, it may be a name)
;;; Purpose:
;;;   Converts the parameter to a value in the range [0..1)
(define env-param
  (lambda (env p)
    (cond
      ((eq? p 'x)
       (/ (pen-x (env-pen env)) (image-width (env-image env))))
      ((eq? p 'y)
       (/ (pen-y (env-pen env)) (image-height (env-image env))))
      ((eq? p 'n)
       (env-n env))
      (else
       p))))

; +-----------+-------------------------------------------------------
; | Sketchers |
; +-----------+


(define sketcher-new
  (lambda params
    (if (null? params)
        (vector 0 ; x
                0 ; y
                0 ; theta
                (vector 100 'sketch.noop) ; instructions
                10 ; max block
                10 ; max length
                10 ; max loop
                )
        (list->vector params))))


(define sketcher-x (object-field-getter 0))

(define sketcher-set-x! (object-field-setter 0))

(define sketcher-y (object-field-getter 1))

(define sketcher-set-y! (object-field-setter 1))

(define sketcher-theta (object-field-getter 2))

(define sketcher-set-theta! (object-field-setter 2))

(define sketcher-instructions (object-field-getter 3))

(define sketcher-set-instructions! (object-field-setter 3))

(define sketcher-maxblock (object-field-getter 4))

(define sketcher-set-maxblock! (object-field-setter 4))

(define sketcher-maxlen (object-field-getter 5))

(define sketcher-set-maxlen! (object-field-setter 5))

(define sketcher-maxloop (object-field-getter 6))

(define sketcher-set-maxloop! (object-field-setter 7))

(define sketcher-code-length 
  (lambda (sketcher)
    (/ (vector-length (sketcher-instructions sketcher) 2))))



;;; Procedure:
;;;   sketcher-instruction!
;;; Parameters:
;;;   sketcher, a sketcher
;;;   env, an environment
;;;   i, an integer
;;;   eval, a boolean
;;; Purpose:
;;;   Evaluate (or pretend to evaluate) a sketcher.  Returns the index of the
;;;   next instruction.
;;; Produces:
;;;   next, an integer
;;; Preconditions:
;;;   0 <= i < (sketcher-code-length sketcher)
;;; Philosophy:
;;;   The core of both sketcher-eval-instruction! and sketcher-next
(define sketcher-instruction!
  (lambda (sketcher env i eval)
    (let* ((instructions (sketcher-instructions sketcher))
           (command (vector-ref instructions (* 2 i)))
           (p (vector-ref instructions (+ 1 (* 2 i))))
           (len (sketcher-code-length sketcher)))
      (cond
        ((eq? command 'sketch.block)
         (let kernel ((remaining (* (env-param env p) (sketcher-maxblock)))
                      (current (+ i 1)))
           ; If we've run out of instructions, we're done
           (cond
             ((>= current len)
              current)
             ((= remaining 0)
              current)
             (else
              (kernel (- remaining 1)
                      (sketcher-instruction! sketcher env current eval))))))
        ((eq? command 'sketch.
        

(define 




; +-------------+------------------------------------------------------
; | Experiments |
; +-------------+

; Experiment 00: Tests pen forward and pen-turn
(define expt-00
  (lambda ()
    (let* ((image (image-show (image-new 100 100)))
           (pen (pen-new 50 50 0 #t)))
      (pen-forward! pen image .65)
      (pen-turn! pen image .25)
      (pen-forward! pen image .75)
      (list image pen))))


; Our first sketch - Just forward and turn
(define sketch-00
  (sketch-new 0 0
              (/ pi 2)
              (vector 'sketch.forward 0.5
                      'sketch.turn 0.6
                      'sketch.forward 0.7)))

; A sketch with one loop
(define sketch-01
  (sketch-new 1 1
              (/ pi 2)
              (vector 'sketch.loop 0.3
                      'sketch.forward 0.2
                      'sketch.loop 0.5
                      'sketch.loop 0.4
                      'sketch.forward 0.3
                      'sketch.turn 0.4)))
