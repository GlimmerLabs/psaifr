;;; File:
;;;   vector-utils.scm
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A collection of utilities for manipulating vectors in Scheme.
;;; Contents:
;;;   (vector-index-of-largest vec len may-precede?)
;;;     Find the index of the largest element in the first len elements of vec.
;;;   (vector-selection-sort! vec may-precede?)
;;;     Sort the vector using selection sort.

;;; Procedure:
;;;   vector-index-of-largest
;;; Parameters:
;;;   vec, a vector
;;;   len, an integer
;;;   may-precede?, a function
;;; Purpose:
;;;   Find the index of the largest element of the vector.
;;; Produces:
;;;   index, an integer
;;; Preconditions:
;;;   For all i,j, 0 <= i,j < len
;;;     (may-precede? (vector-ref vec i) (vector-ref vec j))
;;;     returns true or false (and doesn't crash).
(define vector-index-of-largest
  (lambda (vec len may-precede?)
    (let kernel ((guess 0)
                 (i 1))
      (cond
        ((= i len) 
         guess)
        ((may-precede? (vector-ref vec guess) (vector-ref vec i))
         (kernel i (+ i 1)))
        (else
         (kernel guess (+ i 1)))))))

;;; Procedure:
;;;   vector-swap!
;;; Parameters:
;;;   vec, a vector
;;;   i, an integer
;;;   j, an integer
;;; Purpose:
;;;   Swap the elements in positions i and j
;;; Produces:
;;;   vec, the same vector
;;; Preconditions:
;;;   0 <= i,j < (vector-length vec)
;;;   A == (vector-ref vec i)
;;;   B == (vector-ref vec j)
;;; Postconditions:
;;;   B == (vector-ref vec i)
;;;   A == (vector-ref vec j)
;;;   No other elements are affected
(define vector-swap!
  (lambda (vec i j)
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)
      vec)))

;;; Procedure:
;;;   vector-selection-sort!
;;; Parameters:
;;;   vec, a vector
;;;   may-precede?, a function
;;; Purpose:
;;;   Sort vec.
;;; Produces:
;;;   vec, the same vector, now rearranged
;;; Preconditions:
;;;   For all i,j, 0 <= i,j < (vector-length vec)
;;;     (may-precede? (vector-ref vec i) (vector-ref vec j))
;;;     returns true or false (and doesn't crash).
;;; Postconditions:
;;;   For all i, 1 <= i < (vector-length vec)
;;;     (may-precede? (vector-ref vec (- i 1)) (vector-ref vec i))
;;;   The result is a permutation of the original vector.
(define vector-selection-sort!
   (lambda (vec may-precede?)
     (let kernel ((len (vector-length vec)))
       (when (> len 0)
         (vector-swap! vec
                       (- len 1)
                       (vector-index-of-largest vec len may-precede?))
         (kernel (- len 1))))
     vec))
