(current-directory (string->path "/Users/rebelsky/Art/Genetic"))

(load "./vector-utils.scm")

(define vec1 (vector 1 5 2 1 0))
(vector-index-of-largest vec1 5 <)
(vector-index-of-largest vec1 5 >)
(vector-swap! vec1 1 4)

(vector-selection-sort! vec1 <)
(vector-selection-sort! vec1 >)