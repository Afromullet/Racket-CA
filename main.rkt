#lang racket

(require racket/include)

(include "CA_Transform.rkt")
(include "CA_Draw.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Test Data                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-matrix2 (randomMatrix 50 50))
(define neigh-size 1)

(define ca-stream2
  ((create-ca-matrix-stream neigh-size Rule-90) test-matrix2))

(draw-main ca-stream2)


