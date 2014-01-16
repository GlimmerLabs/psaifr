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
           (x1 (+ x0 (* (cos theta) amt MAX-LEN)))
           (y1 (+ y0 (* (sin theta) amt MAX-LEN)))
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
    (pen-set-theta! pen (+ (pen-theta pen) (* 2 pi amt)))))

(define pen-noop!
  (lambda (pen image amt)
    void))
      


; +----------+--------------------------------------------------------
; | Sketches |
; +----------+

(define param
  (lambda (info)
    (cond
      ((eq? info 'x)
       '(/ (pen-x pen) (image-width image)))
      (else
       info))))

(define sketch-new
  (lambda params
    (if (null? params)
        (vector 0 ; x
                0 ; y
                0 ; theta
                (vector 100 'sketch.noop)
                (vector 100 0))
        (list->vector params))))


(define sketch-x (object-field-getter 0))

(define sketch-set-x! (object-field-setter 0))

(define sketch-y (object-field-getter 1))

(define sketch-set-y! (object-field-setter 1))

(define sketch-theta (object-field-getter 2))

(define sketch-set-theta! (object-field-setter 2))

(define sketch-instructions (object-field-getter 3))

(define sketch-set-instructions (object-field-setter 3))

(define sketch->code
  (lambda (sketch)
    (list 'lambda
          (list 'image 'n)
          (cons 'let
                (cons '((pen (pen-new (sketch-x sketch) 
                                      (sketch-y sketch)
                                      (sketch-theta sketch)
                                      #t)))
                      (subsketch->code (sketch-instructions sketch)
                                       0
                                       (- (vector-length (sketch-instructions sketch)) 1)))))))

;;; Procedure:
;;;   subsketch->code
;;; Parameters:
;;;   instructions, a list of instructions
;;;   pos, an integer
;;; Purpose:
;;;   Convert one instruction (potentially compound) into code
;;; Produces:
;;;   (code . next), a dotted pair
(define instruction->code
  (let ((basic-instructions (list 'sketch.forward 'sketch.noop 
                                  'sketch.pen 'sketch.size 'sketch.turn)))
    (lambda (instructions pos)
      (let ((instruction (vector-ref instructions pos)))
        (cond
          ((member? instruction basic-instructions)
           (cons (list instruction 'pen 'image (param (vector-ref instructions (+ pos 1))))
                 (+ pos 2)))
          ((eq? instruction 'sketch.loop)
           (let ((next (instruction->code instructions (+ pos 2))))
             (cons (list 'loop (unit-scale (param (vector-ref instructions (+ pos 1))) MAX-LOOP)
                         (list 'lambda '() (car next)))
                   (cdr next))))
          (else
           (cons (list 'sketch.noop 'pen 'image (param (vector-ref instructions (+ pos 1))))
                 (+ pos 2))))))))

(define subsketch->code
  (lambda (instructions first last)
    (if (> first last)
        '()
        (let ((stuff (instruction->code instructions first)))
              (cons (car stuff)
                    (subsketch->code instructions (cdr stuff) last))))))


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
