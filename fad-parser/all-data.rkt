#lang racket

(require "pages-to-parsed-tr.rkt")

(provide/contract
          [parsed-qtrs (listof Parsed?)]
          [parsed-qtr-atoms (listof (listof (listof any/c)))]
          [qtr-nums (listof number?)]
          #;[all-dept-names (listof string?)])

;; this file reads in all of the FADs that we have, and 
;; provides them as a list of depts.

(define FAD-DIRECTORY (build-path "/Users/clements/clements/datasets/FAD"))


;; let's find bugs in this file:
#;(file->parsed (build-path FAD-DIRECTORY "fad-2168.txt")
              'post-2142)

#;(/ 1 0)

;; the first quarter to proces
(define FIRST-QTR 2088)
;; the last quarter to process
(define LAST-QTR 2168)

;; these are the "in session" quarters (winter,spring,fall) for
;; which the fad is generated
(define MAIN-QTR-ENDINGS (list 2 4 8))

(define qtr-nums
  (for/list ([qtr (in-range FIRST-QTR (add1 LAST-QTR))]
             #:when (member (modulo qtr 10)
                            MAIN-QTR-ENDINGS))
    qtr))

;; what's the format of this quarter?
(define (qtr->fformat cpqtr)
  (cond [(< cpqtr 2144) 'pre-2144]
        [else 'post-2142]))

(define filenames
  (for/list ([f qtr-nums])
    (build-path FAD-DIRECTORY (~a "fad-"f".txt"))))

(define parsed-qtrs
  (map file->parsed filenames
       (map qtr->fformat qtr-nums)))

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

