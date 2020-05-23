;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Utility Functions                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (permutations size elements)
  (if (zero? size)
      '(())
      (append-map (lambda (p)
                    (map (lambda (e)
                           (cons e p))
                         elements))
                  (permutations (sub1 size) elements))))

;Gets all permutations of a bit-list, where list-size is the number of elements.
;Returns a list of bits, where each entry is one of the permutations
(define (get-bit-permutations list-size) (permutations  list-size'(0 1)))

(define (unfold f init)
  (stream-cons init (unfold f (f init))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                List and Matrix Generation Functions                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-row length)
  (cond
    [(= length 0) empty]
    [else
     (cons (random 2)
           (random-row (- length 1) ))]))

(define (randomMatrix rowLength numCols)
  (cond
    [(= numCols 0) empty]
    [else
     (cons (random-row rowLength) (randomMatrix rowLength (- numCols 1)))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Get Neighbor Related Functions                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Gets up to num-neighs of right neighbors of the start-index
;original-length isn't neeeded just yet, but I am leaving it here in case something comes up
(define (get-right-neighs bit-row start-index num-neighs original-length)
  (cond
    [(= num-neighs 0) empty]
    [else
     (cons (sequence-ref bit-row (+ start-index 1)) (get-right-neighs bit-row (+ start-index 1) (- num-neighs 1) original-length ))]))

;Gets up to num-neighs of left neighbors of the start-index.  Because sequence-ref doesn't work with values less than 0, we have to adjust the start-index if it is equal to 0
;That's why we pass in the original length of the list
(define (get-left-neighs bit-row start-index num-neighs original-length)
  (cond
    [(= num-neighs 0) empty]
    [(= start-index 0) (get-left-neighs bit-row (+ start-index original-length) num-neighs  original-length) ]
    [else
     (cons (sequence-ref bit-row (- start-index 1)) (get-left-neighs bit-row (- start-index 1) (- num-neighs 1)  original-length ))]))

;Need to reverse the left side of the list so that the left neighbors are in the correct order
(define (get-neighs bit-row start-index num-neighs original-length)
  (append (reverse (get-left-neighs bit-row start-index num-neighs original-length))  (list (sequence-ref bit-row start-index)) (get-right-neighs bit-row start-index num-neighs original-length)))

;Gets the neighbor of every bit in the list
(define (get-all-neighs bit-row num-neighs)
  (for/list ([i (in-range (length bit-row))])
    (get-neighs (in-cycle bit-row) i num-neighs(length bit-row))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Structs and struct functions                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;A mapping rule. Maps an input state to an output state
;I.E, 0 1 0 = > 1
(struct Rule (input-state output-state))

(define (build-random-ruleset num-neighs)
  (define bit-permutations (get-bit-permutations num-neighs))
  (for/list ([el bit-permutations])
    (Rule el (random 2))))

(define (print-ruleset rule-list)
  (for([rule rule-list])
    (write (Rule-input-state rule))
    (write " => " )
    (write (Rule-output-state rule))
    (writeln "")))

;Determines whether the given bitlist matches the rule
(define (is-mapping? bit-list rule)
  (if (equal? bit-list (Rule-input-state rule)) #t #f))

;Finds the correct mapping for a given bit-list. The rule-list is a list of mapping rules
(define (get-rule-mapping neighbors rule-list)
  (define cur-rule (first rule-list))
  (cond
    [(is-mapping? neighbors cur-rule) (Rule-output-state cur-rule)]
    [else (get-rule-mapping neighbors (rest rule-list))]))
    
    
(define (map-ruleset neighbor-list rule-list)
  (for/list ([neighs neighbor-list])
    (get-rule-mapping neighs rule-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Pre-existing rulesets                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Rule-30(list
                (Rule '(1 1 1) 0) (Rule '(1 1 0) 0) (Rule '(1 0 1) 0) (Rule '(1 0 0) 1)
                (Rule '(0 1 1) 1) (Rule '(0 1 0) 1) (Rule '(0 0 1) 1) (Rule '(0 0 0) 0)))

