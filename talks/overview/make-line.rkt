#lang racket
(require gigls/unsafe)
(define canvas (image-show (image-new 1 160)))
(context-set-fgcolor! "white")
(image-fill! canvas)
(image-select-nothing! canvas)
(image-save canvas "/tmp/line.png")