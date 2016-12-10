#lang racket

;; apparently this file plots some approximate retention numbers, based on the FAD data

(require plot)

(define including-123-data
  '((2084 0 416 282 216)
    (2088 0 358 198 158)
    (2092 0 142 74 112)
    (2094 0 498 384 284)
    (2098 0 408 136 120)
    (2102 0 132 70 76)
    (2104 0 62 110 66)
    (2108 412 436 376 296)
    (2112 0 274 136 98)
    (2114 0 68 70 104)
    (2118 604 658 526 0)))

(define series-data
  '((2088 416 282 216)
    #;(2092 358 198 158)
    #;(2094 142 74 112)
    (2098 498 384 284)
    #;(2102 408 136 120)
    #;(2104 132 70 76)
    #;(2108 62 110 66)
    (2112 436 376 296)
    #;(2114 274 136 98)
    #;(2118 68 70 104)
    (2122 658 526 0)))

(plot-file
 
 #:x-label "cohort size in 101/2/3"
 #:y-label "total enrollment"
 (for/list ([d including-123-data] [i (in-naturals)])
   (discrete-histogram (list (vector 123 (second d))
                             (vector 101 (third d))
                             (vector 102 (fourth d))
                             (vector 103 (fifth d)))
                       #:label (number->string (first d))
                       #:color i
                       #:x-min (* i 5)))
 "/tmp/retention-incl-123.pdf")