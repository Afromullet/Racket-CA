#lang racket

(require racket/include)
 (require racket/stream)

(include "CA_Transform.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Test Data                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define neighborhood-size 1)

(define test-row '(0 1 1 0 1 1 1 0 0 0 1 0 1 0 1))


(define rand-matrix (randomMatrix 10  3))

(define ((row-apply-rules neighs-size ruleset) input-row)
  (map-ruleset (get-all-neighs input-row neighs-size) ruleset))

(define ((matrix-apply-rules neigh-size ruleset) matrix)
  (cond
    [(empty? matrix) empty]
    [else
     (cons
      (unfold (row-apply-rules neighborhood-size Rule-30) (first matrix))
      ((matrix-apply-rules neigh-size ruleset) (rest matrix))
      )]))


      


(define ca-stream
  (unfold (row-apply-rules neighborhood-size Rule-30) test-row))


(define ca-stream2
  ((matrix-apply-rules neighborhood-size Rule-30) rand-matrix))

