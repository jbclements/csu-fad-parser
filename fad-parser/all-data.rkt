#lang racket

(require "pages-to-parsed-tr.rkt"
         "one-quarter-data.rkt")

(provide/contract
          [parsed-qtrs (listof Parsed?)]
          [parsed-qtr-atoms (listof (listof (listof any/c)))]
          [qtr-nums (listof number?)]
          #;[all-dept-names (listof string?)])

;; this file reads in all of the FADs that we have, and 
;; provides them as 'Parsed's

;; the first quarter to proces
(define FIRST-QTR 2088)
;; the last quarter to process
(define LAST-QTR 2172)

;; these are the "in session" quarters (winter,spring,fall) for
;; which the fad is generated
(define MAIN-QTR-ENDINGS (list 2 4 8))

(define qtr-nums
  (for/list ([qtr (in-range FIRST-QTR (add1 LAST-QTR))]
             #:when (member (modulo qtr 10)
                            MAIN-QTR-ENDINGS))
    qtr))

(define parsed-qtrs
  (map qtr->parsed qtr-nums))

(define parsed-qtr-atoms
  (map Parsed-atoms parsed-qtrs))

#;(define all-dept-names
  (remove-duplicates
   (for*/list ([qtr parsed-qtrs]
               [dept (Parsed-depts qtr)])
     (Dept-name dept))))

#;(define scu-sets
(for*/list ([qtr parsed-qtrs]
            [dept (Parsed-depts qtr)]
            [instructor (Dept-instructors dept)]
            [course (instructor-courses instructor)]
            #:when (and
                    (eq? (line-kind course) 'class)
                    (list? (col-ref 'scu course))
                    (< 1 (length (col-ref 'scu course)))))
  (col-ref 'scu course)))

