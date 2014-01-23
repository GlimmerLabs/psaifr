#lang racket
(require gigls/unsafe)

; venn-diagram
;   Make a simple venn diagram for my talk

;;; Procedure:
;;;   select-circle!
;;; Parameters:
;;;   image, an image
;;;   modifier, one of ADD, SUBTRACT, INTERSECT, REPLACE
;;;   x, an integer
;;;   y, an integer
;;;   size, an integer
;;; Purpose:
;;;   Select the circle centered at (x,Y)
(define select-circle!
  (lambda (image mod x y size)
    (image-select-ellipse! image mod
                           (- x size) (- y size)
                           (+ size size) (+ size size))))

;;; Procedure:
;;;   display-set
;;; Parameters:
;;;   image, an image
;;;   nameTop, a string
;;;   nameBottom, a string
;;;   x, an integer
;;;   y, an integer
;;;   size, an integer
;;;   primary, a color
;;;   secondary, a color
;;; Purpose:
;;;   Represent the given set, with a circle centered at (x,y)
;;; Produces:
;;;   [Nothing, called for the side effect]
(define display-set
  (lambda (image nameTop nameBottom x y size primary secondary)
    (select-circle! image REPLACE x y size)
    (context-set-brush! "2. Hardness 050" 3)
    (context-set-fgcolor! secondary)
    (image-fill! image)
    (context-set-fgcolor! primary)
    (image-stroke! image)
    (image-select-nothing! image)
    (context-set-font-size! 16)
    (context-set-font-name! "Sans Bold")
    (context-set-fgcolor! "black")
    (image-display-text! image nameTop x (- y 5) ALIGN-CENTER ALIGN-BOTTOM)
    (image-display-text! image nameBottom x (+ y 5) ALIGN-CENTER ALIGN-TOP)
    image))

;;; Procedure:
;;;   irgb-join
;;; Parameters:
;;;   c1, an integer-encoded RGB color
;;;   c2, an integer-encoded RGB color
;;; Purpose:
;;;   Create a new color by joining the two colors
(define irgb-join
  (lambda (c1 c2)
    (apply irgb (map max (irgb->rgb-list c1) (irgb->rgb-list c2)))))

;;; Procedure:
;;;   sams-diagram
;;; Purpose:
;;;   Make my diagram
(define sams-diagram
  (lambda ()
    (let* ([diagram (image-new 500 500)]
           [other 224]
           [fp-x 140]
           [fp-y 150]
           [fp-primary (irgb 255 0 0)]
           [fp-secondary (irgb 255 other other)]
           [im-x 360]
           [im-y 150]
           [im-primary (irgb 0 255 0)]
           [im-secondary (irgb other 255 other)]
           [cs-x 250]
           [cs-y 340]
           [cs-primary (irgb 0 0 255)]
           [cs-secondary (irgb other other 255)]
           [size 140])
      
      ; Draw the main sets
      (display-set diagram "Functional" "Programming" fp-x fp-y size
                   fp-primary fp-secondary)
      (display-set diagram "Image" "Making" im-x im-y size
                   im-primary im-secondary)
      (display-set diagram "CompSci" "Outreach" cs-x cs-y size
                   cs-primary cs-secondary)
      
      ; Draw the intersections
      (select-circle! diagram REPLACE fp-x fp-y size)
      (select-circle! diagram INTERSECT im-x im-y size)
      (context-set-fgcolor! (irgb-join fp-secondary im-secondary))
      (image-fill! diagram)
      
      (select-circle! diagram REPLACE fp-x fp-y size)
      (select-circle! diagram INTERSECT cs-x cs-y size)
      (context-set-fgcolor! (irgb-join fp-secondary cs-secondary))
      (image-fill! diagram)
      
      (select-circle! diagram REPLACE im-x im-y size)
      (select-circle! diagram INTERSECT cs-x cs-y size)
      (context-set-fgcolor! (irgb-join im-secondary cs-secondary))
      (image-fill! diagram)
      
      (select-circle! diagram REPLACE fp-x fp-y size)
      (select-circle! diagram INTERSECT im-x im-y size)
      (select-circle! diagram INTERSECT cs-x cs-y size)
      (context-set-fgcolor! "white")
      (image-fill! diagram)
      
      (context-set-font-name! "Serif Bold")
      (context-set-font-size! 24)
      (context-set-fgcolor! "black")
      (image-display-text! diagram "Glimmer" 250 210 ALIGN-CENTER ALIGN-CENTER)
      
      ; Refresh the outsides of the circles
      (context-set-fgcolor! fp-primary)
      (select-circle! diagram REPLACE fp-x fp-y size)
      (image-stroke! diagram)
      (context-set-fgcolor! im-primary)
      (select-circle! diagram REPLACE im-x im-y size)
      (image-stroke! diagram)
      (context-set-fgcolor! cs-primary)
      (select-circle! diagram REPLACE cs-x cs-y size)
      (image-stroke! diagram)
      
      ; Clean up
      (image-select-nothing! diagram)
      diagram)))

