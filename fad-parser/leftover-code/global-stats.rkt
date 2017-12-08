#lang racket

;; yikes, this file isn't in good shape.





#;(
   (require "../all-data.rkt"
         plot)
   
(define (is-overload? c)
  (and (eq? (col-ref 'kind c) 'special)
         (equal? (col-ref 'special c)
                 "COURSE OVERLOAD")))

(define (has-course-overload? instructor)
  (ormap is-overload? (instructor-courses instructor)))

(define (course-overloads instructor)
  (filter is-overload? (instructor-courses instructor)))

(define table
  (group-by
   (for*/list ([qtr parsed-qtrs]
               [dept (parsed-depts qtr)]
               [instr (dept-instructors dept)]
               #:when (member (instructor-name instr)
                              instructors-with-overload))
     (list qtr (dept-name dept) (instructor-name instr)
           (string->number/0 (col-ref 'direct-wtu (un-labwash instr)))))
   third))

#;(for/list ([(name records) table])
  (for/list ([r records])
    )
  (list i (map rest (hash-ref table qtr empty)))))



#|
(define data
  (for/list ([name all-dept-names])
    (list name
          (sort
           (apply
            append
            (for/list ([qtr parsed-qtrs])
              (define dept (list-find name (parsed-depts qtr)
                                      #:key dept-name))
              (for/list ([instructor (dept-instructors dept)])
                (define cleaned (un-labwash instructor))
                (define header (instructor-header cleaned))
                (define IAF (string->number/0
                             (col-ref 'iaf header)))
                (define OSF (string->number/0
                             (col-ref 'osf header)))
                (define IFF (- (string->number/0
                                (col-ref 'tsf header))
                               IAF OSF))
                (/ (string->number/0
                    (col-ref 'total-wtu (instructor-summary
                                         cleaned)))
                   (max 0.001 IFF)))))
           <))))


(define (mean l) 
  (/ (apply + l) (length l)))

;; it's already sorted....
(define (median l)
  (list-ref l (round (/ (length l) 2))))

(define sorted-data
  (sort data >
        #:key (compose median second)))

(plot-file
 #:title "distribution of wtus"
 #:y-label "department"
 #:x-label "wtus"
 #:width 800
 #:height 500
 #:x-max 25.0
 (apply
  mix
  (for/list ([t sorted-data] [i (in-range (length sorted-data) 0 -1)]
                             [color-idx (in-naturals)])
    (define sorted-vals (sort (second t) <))
    (define q-size (/ (length sorted-vals) 4))
    (define first-quartile (list-ref sorted-vals (round q-size)))
    (define second-quartile (list-ref sorted-vals (round (* 2 q-size))))
    (define third-quartile (list-ref sorted-vals (round (* 3 q-size))))
    (mix
     (rectangles
      #:label (first t)
      #:color color-idx
      (list (vector (ivl first-quartile second-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl second-quartile third-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))))
     (points
      #:color color-idx
      (list (vector (first sorted-vals) i)
            (vector (last sorted-vals) i))))))
 "/tmp/faculty-tmp.pdf")

#;(define (stats->csv stats filename)
  (with-output-to-file filename
    (lambda ()
      (for ([l stats])
        (map display (add-between l ", "))
        (display "\n")))))

#;(define (process-file in-file out-file)
  (stats->csv (grouped-pages->stats (read-and-group in-file)) out-file))

(define (idx->date i)
  (list-ref dates i))

#;(define (stats->file data title y-axis-label filename)
  (parameterize ([plot-x-ticks (date-ticks)])
  (plot-file
   #:title title
   #:y-label y-axis-label
   #:x-label "quarter"
   #:width 800
   #:height 500
   (apply
    mix
    (for/list ([t data] [color-idx (in-naturals)])
      (lines
       #:label (dept-short-name (first t))
       #:color color-idx
       (for/list ([pt (second t)] [i (in-naturals)])
         (vector (idx->date i) pt)))))
   filename)))

#;(one-stat->file
 groupeds "TOTAL" 'ftef
 "total ftef by department over time"
 "ftef"
 "/tmp/ftef.pdf")

#;(one-stat->file groupeds "TOTAL" 'scu-per-ftef
          "total scu/ftef by department over time"
          "scu/ftef"
          "/tmp/scu-per-ftef.pdf")

#;(one-stat->file groupeds "TOTAL" 'wtu-per-ftef
          "total wtu/ftef by department over time"
          "wtu/ftef"
          "/tmp/wtu-per-ftef.pdf")

#|(define scu-per-ftef (one-stat groupeds "TOTAL" 'scu-per-ftef))
(define wtu-per-ftef (one-stat groupeds "TOTAL" 'wtu-per-ftef))
(define scu-per-wtu
  (for/list ([s scu-per-ftef] [w wtu-per-ftef])
    (unless (equal? (first s) (first w))
      (error (~a"names differ: "(first s)" and "(first w))))
    (list (first s)
          (map / (second s) (second w)))))
(stats->file scu-per-wtu "scu/wtu by department over time"
             "scu/wtu"
             "/tmp/scu-per-wtu.pdf")|#

|#