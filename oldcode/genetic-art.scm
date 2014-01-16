;;; File:
;;;   genetic-art.scm
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A basic exploration of genetic art.

; +---------+----------------------------------------------------------
; | Modules |
; +---------+

(current-directory (string->path "/Users/rebelsky/Art/Genetic"))
(load "./vector-utils.scm")


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
;;;   genetic-image
;;; Parameters:
;;;   rfun, a function from three values (x, y, n) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   gfun, a function from three values (x, y, n) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   bfun, a function from three values (x, y, n) in the range
;;;     [-1.0 ... 1.0] to a value in the same range.  
;;;   width, a positive integer
;;;   height, a positive integer
;;;   n, an integer in the range [-1.0 .. 1.0]
;;; Purpose:
;;;   Generate a new image of the given width and height by applying
;;;   the functions at each position.
;;; Produces:
;;;   img, an image.
(define genetic-image
  (lambda (rfun gfun bfun width height n)
    (let ((half-width (/ width 2.0))
          (half-height (/ height 2.0)))
      (image-compute (lambda (x y)
                       (let ((newx (- (/ x half-width) 1.0))
                             (newy (- (/ y half-height) 1.0)))
                         (rgb-new (range2component (rfun newx newy n))
                                  (range2component (gfun newx newy n))
                                  (range2component (bfun newx newy n)))))
                     width height))))

;;; Function:
;;;   body2fun
;;; Parameters:
;;;   body, an s expression
;;; Purpose:
;;;   Given the body of a component function, returns the component function.
;;; Produces:
;;;   fun, a function
(define body2fun
  (lambda (body)
    (eval (list 'lambda (list 'x 'y 'n) body))))

(define cap
  (lambda (val)
    (max -1.0 (min 1.0 val))))

(define wrap
  (lambda (val)
    (cond
      ((< val -1.0)
       (wrap (+ val 2.0)))
      ((> val 1.0)
       (wrap (- val 2.0)))
      (else
       val))))

; +-----------------+-------------------------------------------------
; | Building Blocks |
; +-----------------+

(define bb.add1
  (lambda (a b) (cap (+ a b))))

(define bb.add2
  (lambda (a b) (wrap (+ a b))))

(define bb.ave
  (lambda (a b) (/ (+ a b) 2)))

(define bb.sin
  (o sin (l-s * pi)))

(define bb.cos
  (o cos (l-s * pi)))

(define binary
  (list 'bb.add1 'bb.add2 'bb.ave '* 'max 'min))
  
(define unary
  (list 'bb.sin 'bb.cos 'square 'abs '-))

(define zeroary
  (list 'x 'y 'n 'x 'y 'n 'x 'y 'n -1 -0.5 0 0.5 1))


; +---------+---------------------------------------------------------
; | Artists |
; +---------+

; The things that do the drawing.  They have red functions, blue functions,
; green functions, and values.
; TODO - Add sanity checking.
(define artist
  (lambda (_name . extra)
    (let ((name _name)
          (rfun (if (null? extra) 0 (car extra)))
          (gfun (if (null? extra) 0 (cadr extra)))
          (bfun (if (null? extra) 0 (caddr extra)))
          (value 0))
      (lambda (msg . args)
        (cond 
          ((eq? msg ':set-rfun!)
           (set! rfun (car args)))
          ((eq? msg ':set-gfun!)
           (set! gfun (car args)))
          ((eq? msg ':set-bfun!)
           (set! bfun (car args)))
          ((eq? msg ':set-value!)
           (set! value (car args)))
          ((eq? msg ':bfun)
           bfun)
          ((eq? msg ':gfun)
           gfun)
          ((eq? msg ':rfun)
           rfun)
          ((eq? msg ':name)
           name)
          ((eq? msg ':value)
           value)
          (else
           (error "Invalid message" msg)))))))

(define random-artist
  (lambda (name)
    (let ((a (artist name)))
      (a ':set-rfun! (random-fun 0))
      (a ':set-gfun! (random-fun 0))
      (a ':set-bfun! (random-fun 0))
      a)))

(define artist-boring?
  (lambda (artist)
    (> 8 (+ (depth (artist ':rfun)) 
            (depth (artist ':gfun))
            (depth (artist ':bfun))))))

(define random-interesting-artist
  (lambda (name)
    (let ((a (random-artist name)))
      (if (artist-boring? a)
          (random-interesting-artist name)
          a))))

(define artist-save
  (lambda (artist file)
    (let ((port (open-output-file file 'replace)))
      (artist-print artist port)
      (close-output-port port)
      artist)))

(define artist-print
  (lambda (artist port)
    (print (artist ':name) port)
    (newline port)
    (print (artist ':value) port)
    (newline port)
    (print (artist ':rfun) port)
    (newline port)
    (print (artist ':gfun) port)
    (newline port)
    (print (artist ':bfun) port)
    (newline port)
    artist))
      
(define artist-load
  (lambda (fname)
    (let* ((port (open-input-file fname))
           (a (artist-read port)))
      (close-input-port port)
      a)))

(define artist-read
  (lambda (port)
    (let ((name (read port)))
      (if (eof-object? name)
          #f
          (let ((a (artist name)))
            (a ':set-value! (read port))
            (a ':set-rfun! (read port))
            (a ':set-gfun! (read port))
            (a ':set-bfun! (read port))
            a)))))

(define artists-load
  (lambda (fname)
    (let ((port (open-input-file fname)))
      (let kernel ((artists null))
        (let ((a (artist-read port)))
          (if a
              (kernel (cons a artists))
              (reverse artists)))))))

(define artist-draw
  (lambda (artist width height n)
    (genetic-image (body2fun (artist ':rfun))
                   (body2fun (artist ':gfun)) 
                   (body2fun (artist ':bfun))
                   width height
                   n)))

; Select one of the color functions
(define select-colorfun
  (lambda (primary secondary tertiary)
    (cond
      ((< (random) 0.90)
       primary)
      ((< (random) 0.50)
       secondary)
      (else
       tertiary))))

; Combine two colurfuns
(define breed-colorfuns
  (lambda (fun1 fun2)
    (let ((r (random)))
      (cond
        ; 20% of the time, we just get the first function
        ((< r 0.20)
         fun1)
        ; 20% of the time, we just get the second function
        ((< r 0.40)
         fun2)
        ; 20% of the time, we combine them with a random binary
        ((< r 0.60)
         (list (random-elt binary) fun1 fun2))
        ; 20% of the time, we breed with f1 as primary
         ((< r 0.80)
          (breed-funs fun1 fun2))
         ; The rest of the time, we breed with f2 as primary
         (else
          (breed-funs fun2 fun1))))))
      
(define breed-artists
  (lambda (name a1 a2)
    (let ((a (artist name))
          (r1 (a1 ':rfun))
          (g1 (a1 ':gfun))
          (b1 (a1 ':bfun))
          (r2 (a2 ':rfun))
          (g2 (a2 ':gfun))
          (b2 (a2 ':bfun)))
      (a ':set-rfun! (breed-colorfuns (select-colorfun r1 g1 b1)
                                      (select-colorfun r2 g2 b2)))
      (a ':set-gfun! (breed-colorfuns (select-colorfun g1 b1 r1)
                                      (select-colorfun g2 b2 r2)))
      (a ':set-bfun! (breed-colorfuns (select-colorfun b1 r1 g1)
                                      (select-colorfun b2 r2 g2)))
      a)))

(define artist-mutant
  (lambda (a name)
    (let ((mutant (artist name)))
      (mutant ':set-rfun! (mutate-fun (mutant ':rfun)))
      (mutant ':set-gfun! (mutate-fun (mutant ':gfun)))
      (mutant ':set-bfun! (mutate-fun (mutant ':bfun)))
      mutant)))

; +----------+--------------------------------------------------------
; | Building |
; +----------+

(define dir "/home/rebelsky/Web/Glimmer/GeneticArt/")

(define zero-prefix
  (lambda (str)
    (string-append (substring "0000" 0 (- 4 (string-length str))) str)))

(define gallery
  (lambda (artist width height)
    (artist-save artist (string-append (artist ':name) ".art"))
    (map (lambda (n)
           (image-save (artist-draw artist width height (- (/ n 500) 1))
                       (string-append (artist ':name)
                                      "-"
                                      (zero-prefix (number->string n))
                                      "-" 
                                      (number->string width)
                                      "x"
                                      (number->string height)
                                      ".jpg")))
         (list 0 50 200 500 750 1000))))

(define artist-samples
  (lambda (artist prefix width height)
    (map (lambda (i)
           (image-save (artist-draw artist width height (- (/ i 500) 1))
                       (string-append prefix (ndigits i 4) ".jpg")))
         (list 0 50 200 500 750 1000))))

(define save
  (lambda (image name)
    (gimp-file-save 1 ; non-interactive
                    image 
                    (image-get-layer image) 
                    (string-append dir name)
                    name)))


(define random-elt
  (lambda (lst)
    (list-ref lst (random (length lst)))))

(define random-fun
  (lambda (level)
    (let ((r (random (+ level 5))))
      (cond
        ((<= r 1)
         (list (random-elt binary)
               (random-fun (+ level 1))
               (random-fun (+ level 1))))
        ((<= r 4)
         (list (random-elt unary)
               (random-fun (+ level 1))))
        (else
         (random-elt zeroary))))))

; Mutate one of our random functions
(define mutate-fun
  (lambda (fun)
    (cond
      ; 90% of the time, we don't mutate the current node (but
      ; might mutate the children)
      ((< (random) 0.9)
       (if (not (pair? fun))
           ; If we're at a leaf, we're done
           (if (< (random) 0.5) (random-elt zeroary) fun)
           ; Otherwise, we might still mutate the children
           (cons (car fun) (map mutate-fun (cdr fun)))))
      ; 2.5% of the time, we add another function (and may still mutate the rest)
      ((< (random) 0.25)
       (list (random-elt unary) (mutate-fun fun)))
      ; The rest of the time, we change the current node (delete, change, ...)
      (else
       ; Once in a while, we mutate the current node
       (let ((r (random)))
         (cond
           ; Leaves
           ((not (pair? fun))
            (cond 
              ; 50% of the time get replaced by another zeroary function
              ((< r 0.50)
               (random-elt zeroary))
              ; 25% of the time have a unary function inserted
              (else
               (list (random-elt unary) fun))))
           ; Unary functions
           ((null? (cddr fun))
            (cond
              ; 50% of the time, we just use another unary function
              ((< r 0.50)
               (list (random-elt unary)
                     (mutate-fun (cdr fun))))
              ; 40% of the time, we lose the unary function
              ((< r 0.90)
               (mutate-fun (cdr fun)))
              ; The rest of the time, we become a binary function
              (else
               (list (random-elt binary)
                     (random-elt zeroary)
                     (mutate-fun (cdr fun))))))
           ; Binary functions
           (else
            (cond
              ; 50% of the time, we just replace the binary function
              ((< r 0.55)
               (list (random-elt binary)
                     (mutate-fun (cadr fun))
                     (mutate-fun (caddr fun))))
              ; 25% of the time, we just get the left subtree
              ((< r 0.75)
               (mutate-fun (cadr fun)))
              ; 25% of the time, we just get the right subtree
              (else
               (mutate-fun (caddr fun)))))))))))
    
    
; Breed two functions
(define breed-funs
  (lambda (left right)
    (randomly-insert left (random-subtree right))))

; Randomly insert one tree into another
(define randomly-insert
  (lambda (tree subtree)
    (cond
      ((not (pair? tree))
       subtree)
      ((zero? (random (tree-height tree)))
       subtree)
      ((equal? (length tree) 2)
       (list (car tree) (randomly-insert (cadr tree) subtree)))
      ; Binary trees
      ((< (random) 0.5)
       (list (car tree) (randomly-insert (cadr tree) subtree) (caddr tree)))
      (else
       (list (car tree) (cadr tree) (randomly-insert (caddr tree) subtree))))))

       

; Find the height of a tree
(define tree-height
  (lambda (tree)
    (if (pair? tree)
        (+ 1 (apply max (map tree-height tree)))
        0)))

; Find a random subtree of a tree.  This version is particularly inefficient.
(define random-subtree
  (lambda (tree)
    (cond
      ; Leaves are the best we can do
      ((not (pair? tree))
       tree)
      ; Probability of taking something as is on its height
      ((zero? (random (tree-height tree)))
       tree)
      (else
       (random-subtree (random-elt (cdr tree)))))))

; +----------+--------------------------------------------------------
; | Genetics |
; +----------+


;;; Procedure:
;;;   check-pixel
;;; Parameters:
;;;   artist, an artist
;;;   pixel, a vector of <x y n color>
;;; Purpose:
;;;   Tells us how close the pixel the artist computes at (x,y) in image n
;;;   is to color.  (Basically, the square of the distance, normalized to 0..1.
(define check-pixel
  (lambda (rfun gfun bfun pixel)
    ; Extract components from the vector
    (let ((x (vector-ref pixel 0))
          (y (vector-ref pixel 1))
          (n (vector-ref pixel 2))
          (color (vector-ref pixel 3)))
      ; Compute the expected and actual colors
      (let ((expected-r (component2range (rgb-red color)))
            (expected-g (component2range (rgb-green color)))
            (expected-b (component2range (rgb-blue color)))
            (actual-r (rfun x y n))
            (actual-g (gfun x y n))
            (actual-b (bfun x y n)))
        ; Find their square differences
        (let ((value (/ (+ (square (- actual-r expected-r))
                           (square (- actual-g expected-g))
                           (square (- actual-b expected-b)))
                        12)))
          ; And report
          value)))))

;;; Procedure:
;;;   average-distance
;;; Parameters:
;;;   distances, a list of distances as computed by check-pixel
(define average-distance
  (lambda (distances)
    (sqrt (/ (apply + distances) (length distances)))))

;;; Procedure:
;;;   make-fitness-pixels
;;; Parameters:
;;;   pixels, a list of <x y n color> vectors
;;; Purpose:
;;;   Make a fitness function that determines the fitness of an artist based on
;;;   how closely it matches the color at x y n.
;;; Produces:
;;;   fitness, a function
(define make-fitness-from-pixels
  (lambda (pixels)
    (lambda (artist)
      (let ((rfun (body2fun (artist ':rfun)))
            (gfun (body2fun (artist ':gfun)))
            (bfun (body2fun (artist ':bfun))))
        (- 1 (average-distance (map (lambda (pixel) 
                                      (check-pixel rfun gfun bfun pixel)) 
                                    pixels)))))))

;;; Procedure:
;;;   evaluate-artists!
;;; Parameters:
;;;   artists, a lists of artists
;;;   fitness, a function from artist to number
;;; Purpose:
;;;   Fills in the value field of all the artists
;;; Produces:
;;;   artists, the same vector (now mutated)
(define evaluate-artists!
  (lambda (artists fitness)
    (list-foreach! artists (lambda (artist) (artist ':set-value! (fitness artist))))
    artists))

;;; Procedure:
;;;   select-artist
;;; Parameters:
;;;   artists, a list of artists
;;;   total-fitness, a real number
;;; Purpose:
;;;   "Randomly" select an artist based on its fitness
;;; Produces:
;;;   a, an artist
;;; Preconditions:
;;;   fitness is the sum of the fitness of the artists
;;;   a is nonempty
;;; Postconditions:
;;;   (member a artists)
;;;   a is difficult to predict
;;;   Over a large sequence of calls, the percentage of time any artist appears
;;;     is proportional to its fitness
(define select-artist
  (lambda (artists total-fitness)
    (let kernel ((remaining artists)
                 (r (random)))
      (if (null? remaining)
          (car artists)
          (let* ((artist (car remaining))
                 (val (/ (artist ':value) total-fitness)))
            ; (display (list 'sa-kernel 'artist (artist ':name) 'r r 'val val)) (newline)
            (if (< r val)
                artist
                (kernel (cdr remaining) (- r val))))))))


;;; Procedure:
;;;   next-generation
;;; Parameters:
;;;   generation, a list of artistss
;;;   size, an integer
;;; Purpose:
;;;   Computes a new generation of artists, where artists are more likely
;;;   to breed if they are more fit.
;;; Produces:
;;;   nextgen, a vector of artists
(define next-generation
  (lambda (generation size prefix)
    (let ((total-value (apply + (map (lambda (artist) (artist ':value)) generation))))
      (append 
       ; One or two random artists
       (list (random-interesting-artist (string-append prefix "000"))
             (random-interesting-artist (string-append prefix "001")))
       ; We use mutants of the top artists
       (list (artist-mutant (car generation) (string-append prefix "002"))
             (artist-mutant (car generation) (string-append prefix "003"))
             (artist-mutant (car generation) (string-append prefix "004"))
             (artist-mutant (cadr generation) (string-append prefix "005"))
             (artist-mutant (cadr generation) (string-append prefix "006"))
             (artist-mutant (caddr generation) (string-append prefix "007")))
       ; The top artists of the generation
       (list (car generation)
             (cadr generation)
             (caddr generation))
       ; And breed for the remaining artists
       (let kernel ((i 10)
                    (kids null))
         (if (>= i size)
             kids
             (let* ((p1 (select-artist generation total-value))
                    (p2 (select-artist generation total-value)))
               (kernel (+ i 1)
                       (cons (breed-artists (string-append prefix (ndigits i 3))
                                            p1 
                                            p2)
                             kids)))))))))

(define artists-write
  (lambda (artists fname)
    (let ((port (open-output-file fname 'replace)))
      (list-foreach! artists (r-s artist-print port))
      (close-output-port port))))

(define artists-sort
  (lambda (artists)
    (vector->list (vector-selection-sort! (list->vector artists)
                                          (lambda (a1 a2) (>= (a1 ':value) (a2 ':value)))))))

(define first-generation
  (lambda (gensize)
    (let kernel ((i 0)
                 (gen null))
      (if (>= i gensize)
          gen
          (kernel (+ i 1)
                  (cons (random-artist (string-append "000." (ndigits i 3)))
                        gen))))))

(define depth
  (lambda (thing)
    (if (pair? thing)
        (+ 1 (apply max (map depth thing)))
        0)))

(define remove-boring
  (lambda (artists)
    (if (null? artists)
        null
        (let ((rest (remove-boring (cdr artists))))
          (if (artist-boring? (car artists))
              rest
              (cons (car artists) rest))))))


;;; Starting with a set of artists, evolve them for numgens generations
;;  using the specified fitnes
(define evolve-artists
  (lambda (worldname artists gensize numgens fitness)
    (let kernel ((year 0)
                 (generation artists))
      (let ((gen (artists-sort (evaluate-artists! (remove-boring generation) fitness))))
        (display "Generation ")
        (display year)
        (display ", best value ")
        (display ((car gen) ':value))
        (newline)
        (artists-write gen (string-append worldname (ndigits year 3)))
        (when (< year numgens)
          (kernel (+ year 1)
                  (next-generation gen gensize (string-append (ndigits year 3) "."))))))))
    
; +----------------------+---------------------------------------------
; | Genetics w/o Fitness |
; +----------------------+

;;; Given artists, make generation n of size size
(define make-generation
  (lambda (artists gen size)
    (letrec ((kernel (lambda (i)
                       (if (= i size)
                           nil
                           (cons (breed-artists (artist-name gen i)
                                                (random-elt artists)
                                                (random-elt artists))
                                 (kernel (+ i 1)))))))
      (kernel 0))))


(define demo-artist
  (lambda (dir fname paintings width height)
    (let ((a (artist-load (string-append dir "/" fname)))
          (html (open-output-file (string-append dir "/" fname ".html") 'replace)))
      (display "<html>" html) (newline html)
      (display "<body>" html) (newline html)
      (map (lambda (i)
             (display (list 'artist-draw artist width height (- (/ i 500) 1))) (newline)
             (image-save (artist-draw a width height (- (/ i 500) 1))
                         (string-append dir "/" fname (ndigits i 4) ".jpg"))
             (display "<img src=\"" html)
             (display (string-append fname (ndigits i 4) ".jpg") html)
             (display "\"/>" html)
             (newline html))
           (map (o round (l-s * (/ 1000 (- paintings 1)))) (iota paintings)))
      (display "</body>" html) (newline html)
      (display "</html>" html) (newline html)
      (close-output-port html))))



