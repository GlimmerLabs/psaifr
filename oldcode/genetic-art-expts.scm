(current-directory (string->path "/Users/rebelsky/Art/Genetic"))

(load "genetic-art.scm")

(define artist-red (artist "red" 1 -1 -1))
(define artist-green (artist "green" -1 1 -1))
(define artist-blue (artist "blue" -1 -1 1))
(define artist-grey (artist "grey" 0 0 0))
(define artist-white (artist "white" 1 1 1))

(define artist1 (artist "artist1" 'x 0 'y))

(define artists (list artist-red artist-green artist-blue artist-grey artist-white artist1))

(define check-artist
  (lambda (artist)
    (image-show (artist-draw artist 100 100 0))))

(define some-pixels
  (list (vector -1 -1 -1 RGB-RED)
        (vector -1 -1  1 RGB-GREEN)
        (vector -1  1 -1 RGB-BLUE)
        (vector -1  1  1 RGB-BLACK)
        (vector  1 -1 -1 RGB-WHITE)))

; red-pixels: Scattered pixels that with n==0 are all red
(define red-pixels
  (list (vector -0.5 -0.5 0 RGB-RED)
        (vector -0.5  0.0 0 RGB-RED)
        (vector -0.5  0.5 0 RGB-RED)
        (vector  0.0 -0.5 0 RGB-RED)
        (vector  0.0  0.0 0 RGB-RED)
        (vector  0.0  0.5 0 RGB-RED)
        (vector  0.5 -0.5 0 RGB-RED)
        (vector  0.5  0.0 0 RGB-RED)
        (vector  0.5  0.5 0 RGB-RED)))

(define red-fitness (make-fitness-from-pixels red-pixels))

; check-pixel assumes that we've extracted the functions.  For testing, we'll do
; it once.
(define checkpix
  (lambda (artist pixel)
    (check-pixel (body2fun (artist ':rfun))
                 (body2fun (artist ':gfun))
                 (body2fun (artist ':bfun))
                 pixel)))

; Some of my original experiments


(define i 0)


(define expt1
  (lambda (width height)
    (image-show (genetic-image (body2fun 'x) (body2fun 'y) (body2fun 'n)
                               width height
                               -1))))

(define expt2
  (lambda (width height n)
    (image-show (genetic-image (body2fun f1) (body2fun f2) (body2fun f3)
                               width height
                               n))))

(define f1 (random-fun 0))
(define f2 (random-fun 0))
(define f3 (random-fun 0))

(define t (list 'a (list 'b (list 'c (list 'd (list 'e 'f) 'a)))))

(define a (random-artist "A"))

(define gen0 (vector (random-artist "A")
                     (random-artist "B")
                     (random-artist "C")
                     (random-artist "D")
                     (random-artist "E")))

(define artist-name
  (lambda (gen num)
    (string-append "A" 
                   (zero-prefix (number->string gen)) 
                   (zero-prefix (number->string num)))))

;;; Create the initial generation
(define gen0
  (map (lambda (num)
         (random-artist (artist-name 0 num)))
       (iota 8)))

(define artist-names
  (l-s map (lambda (a) (a ':name))))

(define galleries
  (lambda (artists width height)
    (map (lambda (artist) (gallery artist width height)) artists)))

; Okay, here's my big test

; First, we need a reasonable target.  We're going for some red/purple blends that vary over time.
(define purple-pixels
  (list 
   ; Smallest n: red/purple/blue horizontal
   (vector -0.5 -0.5 -1 RGB-RED)
   (vector -0.5  0.0 -1 RGB-RED)
   (vector -0.5  0.5 -1 RGB-RED)
   (vector  0.0 -0.5 -1 RGB-PURPLE)
   (vector  0.0  0.0 -1 RGB-PURPLE)
   (vector  0.0  0.5 -1 RGB-PURPLE)
   (vector  0.5 -0.5 -1 RGB-BLUE)
   (vector  0.5  0.0 -1 RGB-BLUE)
   (vector  0.5  0.5 -1 RGB-BLUE)
   
   ; Middle n: red/purple/blue vertical
   (vector -0.5 -0.5  0 RGB-RED)
   (vector -0.5  0.0  0 RGB-PURPLE)
   (vector -0.5  0.5  0 RGB-BLUE)
   (vector  0.0 -0.5  0 RGB-RED)
   (vector  0.0  0.0  0 RGB-PURPLE)
   (vector  0.0  0.5  0 RGB-BLUE)
   (vector  0.5 -0.5  0 RGB-RED)
   (vector  0.5  0.0  0 RGB-PURPLE)
   (vector  0.5  0.5  0 RGB-BLUE)
   
   ; Largest n: blue/purple/red vertical
   (vector -0.5 -0.5  1 RGB-BLUE)
   (vector -0.5  0.0  1 RGB-BLUE)
   (vector -0.5  0.5  1 RGB-BLUE)
   (vector  0.0 -0.5  1 RGB-PURPLE)
   (vector  0.0  0.0  1 RGB-PURPLE)
   (vector  0.0  0.5  1 RGB-PURPLE)
   (vector  0.5 -0.5  1 RGB-RED)
   (vector  0.5  0.0  1 RGB-RED)
   (vector  0.5  0.5  1 RGB-RED)
   ))

; Life may be more interesting if I can set up a somewhat random policy
(define random-pixels
  (let ((rnum (lambda ()
                (* .01 (round (* 100 (- (* 2 (random)) 1.0)))))))
    (lambda (n colors)
      (if (<= n 0)
          null
          (cons (vector (rnum) ; x
                        (rnum) ; y
                        (rnum) ; n
                        (random-elt colors))
                (random-pixels (- n 1) colors))))))

(define random-purple-pixels (random-pixels 36 (list RGB-RED RGB-BLUE RGB-PURPLE RGB-WHITE)))

(define pixels-save 
  (lambda (pixels dir)
    (let ((port (open-output-file (string-append dir "/pixels") 'replace)))
      (display "(" port) (newline port)
      (let kernel ((pixels pixels))
        (when (not (null? pixels))
          (let* ((pixel (car pixels))
                 (color (vector-ref pixel 3)))
            (display (list (vector-ref pixel 0)
                           (vector-ref pixel 1)
                           (vector-ref pixel 2)
                           (rgb-red color)
                           (rgb-green color)
                           (rgb-blue color))
                     port)
            (newline port)
            (kernel (cdr pixels)))))
      (display ")" port) 
      (newline port)
      (close-output-port port))))

(define pixels-read
  (lambda (fname)
    (let* ((port (open-input-file fname))
           (result (read port)))
      (close-input-port port)
      (map (lambda (lst)
             (vector (car lst)
                     (cadr lst)
                     (caddr lst)
                     (rgb-new (list-ref lst 3)
                              (list-ref lst 4)
                              (list-ref lst 5))))
           result))))

(define image-sample-pixel
  (lambda (image n)
    (let ((x (random (image-width image)))
          (y (random (image-height image))))
      (vector (- (/ (* 2.0 x) (- (image-width image) 1)) 1.0) ; x converted
              (- (/ (* 2.0 y) (- (image-height image) 1)) 1.0) ; y converted
              n ; does not need to be converted
              (image-get-pixel image x y)))))
      

; Sample pixels from a single image
(define image-sample-pixels
  (lambda (image numpixels n)
    (let kernel ((pixels null)
                 (i 0))
      (if (>= i numpixels)
          pixels
          (kernel (cons (image-sample-pixel image n) pixels)
                  (+ i 1))))))

; Sample pixels from a bunch of images
(define sample-pixels
  (lambda (images numpixels)
    (let ((len (length images)))
      (apply append
             (map (lambda (image number)
                    (image-sample-pixels image numpixels (- (/ (* 2.0 number) (- len 1)) 1.0))))))))

; Finally, some instructions for evolving them
(define purples
  (lambda ()
    (evolve-artists ("/tmp/Purples/purples")
                    (first-generation 32) 
                    32 64 
                    (make-fitness-from-pixels purple-pixels))
    (let ((a (artist-load "/tmp/Purples/purples064")))
      (artist-samples a "/tmp/Purples/image-" 128 128))))

; Conduct an experiment using a list of pixels in a file.
(define expt
  (lambda (dir generations)
    ; Grab the pixels from the saved file
    (let ((pixels (pixels-read (string-append dir "/pixels"))))
      (evolve-artists (string-append dir "/gen")
                      (first-generation 32)
                      32 ; Generation size
                      generations ; Generations
                      (make-fitness-from-pixels pixels)))
    (let ((artists (artists-load (string-append dir "/gen" (ndigits generations 3)))))
      (map (lambda (pos)
             (display "Generating samples for artist ")
             (display pos)
             (newline)
             (artist-samples (list-ref artists pos) 
                             (string-append dir "/" (number->string pos) "-") 
                             128 128))
           (iota 3)))))

