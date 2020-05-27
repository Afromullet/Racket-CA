;#lang racket

(require racket/include)
(require racket/stream)
(require 2htdp/universe 2htdp/image lang/posn)




(define ALIVE-COLOR "BLACK")
(define DEAD-COLOR "White")
(define SQUARE-SIZE 10)

(define alive-square (square SQUARE-SIZE 'solid ALIVE-COLOR))
(define dead-square (square SQUARE-SIZE 'outline DEAD-COLOR))

(define (get-square state)
  (if (= state 1) alive-square dead-square))

(define (draw-row state-list)
  (cond
    [(empty? state-list) empty-image]
    [else
     (beside (get-square (first state-list)) (draw-row (rest state-list)))]))

(define (draw-matrix state-list)
  (cond
    [(empty? state-list) empty-image]
    [else
     (above (draw-row (first state-list)) (draw-matrix (rest state-list)))]))

(define (draw-main start-state)
(big-bang 
 start-state

 (on-tick
  (lambda (state)
    (get-next-matrix state)) 1)  
 (to-draw
    (lambda (state)
      (draw-matrix (get-first-matrix state))))
  ))





