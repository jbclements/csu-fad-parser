#lang racket

(require db
         plot)

;; something to do with faculty contact hours?
(module+ main
(define conn
   (mysql-connect #:user "clements"
                 #:database "fad"
                 #:port 11306))

;; oh dear... FCH need to be corrected, too.

#;(define pre
  (query-rows
   conn
   (~a
    "SELECT *,(offerings.enrollment * offerseqs.ttf) FROM (offerseqs, offerings) "
    "WHERE offerseqs.qtr = offerings.qtr "
    "AND offerseqs.subject = offerings.subject "
    "AND offerseqs.num = offerings.num "
    "AND offerseqs.section = offerings.section "
    "AND offerings.classification != offerings.classificationcorrected")))


#;(define changed
  (for/list ([p pre])
    (define p-list (vector->list p))
    (append (take p-list 11)
            (list (last p-list))
            (list (list-ref p-list 12)))))


#;(with-output-to-file "/tmp/foo.txt"
  (lambda ()
    (for ([c changed])
      (display (apply ~a (add-between c "\t")))
      (newline))))

#;(for/list ([c (first changed)])
  (query
   conn
   (~a "REPLACE offerseqs VALUE (?)")
   (list->vector c)))


#;(length pre)

#;(define pre
  (query-rows
   conn
   (~a
    "SELECT offerseqs.qtr, offerseqs.subject, offerseqs.num, offerseqs.section, sequence, classification, classificationcorrected, enrollment, fch"
    " FROM (offerseqs,offerings) WHERE "
    "offerseqs.qtr = offerings.qtr " 
    "AND offerseqs.subject = offerings.subject "
    "AND offerseqs.num = offerings.num "
    "AND offerseqs.section = offerings.section "
    "AND offerings.classification != offerseqs.classificationcorrected")))


#;(take pre 3)
#;(length pre)
#;(remove-duplicates
 (for/list ([p pre])
  (list (vector-ref p 1) (vector-ref p 2))))
#;(define (updated-fch row)
  )


#;(query-rows
   conn
   (~a
    "SELECT fchcorrected FROM offerseqs "
    "WHERE instructor = 'C   LUPO' "
    "AND qtr = 2114"))

(define (fch-table extra-clauses)
  (define 
    query-result
    (query-rows
     conn
     (apply 
      ~a
     `("SELECT offerseqs.instructor, offerseqs.qtr, "
      "SUM(offerseqs.fchcorrected) FROM (offerseqs,offerings,instructorstatuses) "
      "WHERE offerseqs.qtr = offerings.qtr "
      "AND offerseqs.subject = offerings.subject "
      "AND offerseqs.num = offerings.num "
      "AND offerseqs.section = offerings.section "
      "AND offerseqs.instructor = instructorstatuses.id "
      "AND offerseqs.qtr = instructorstatuses.qtr "
      ,@extra-clauses
      "GROUP BY offerseqs.instructor, offerseqs.qtr"))))
  
  (display (~a "selected "(length query-result)" rows.\n"))
  
  (define supervisory-table
    (for/hash ([r query-result])
      (values (list (vector-ref r 0) (vector-ref r 1))
              (vector-ref r 2))))
  supervisory-table)


(define taught-but-no-iff
  (query-rows
   conn
   (~a
    "SELECT instructorstatuses.id, instructorstatuses.qtr, "
    "offerseqs.subject, offerseqs.num "
    "FROM (instructorstatuses, offerseqs, tenuretracks) "
    "WHERE (tsf - (iaf + osf)) = 0 "
    "AND instructorstatuses.id = offerseqs.instructor "
    "AND instructorstatuses.qtr = offerseqs.qtr "
    "AND instructorstatuses.id = tenuretracks.id "
    )))

;; ISSUES:

;; HOW TO TREAT SUPERVISORY HOURS? 

;; DISREGARDING COURSES TAUGHT BY 
;; INSTRUCTORS WITH IFF=0
  ;; ... data here deleted.


(define (data-for-group dept-clauses tt-clause)
  (define iff-query
    (query-rows
     conn
     (apply 
      ~a
      `(
        "SELECT instructorstatuses.id, "
        "instructorstatuses.qtr, (instructorstatuses.tsf - "
        "(instructorstatuses.iaf + instructorstatuses.osf)) "
        "FROM instructorstatuses "
        "WHERE "
        ,tt-clause
        ,@dept-clauses
        ))))
  (define iff-table
    (for/hash ([r iff-query])
      (values (list (vector-ref r 0) (vector-ref r 1))
              (vector-ref r 2))))
  
  (define supervisory-table
    (fch-table 
     (append
      dept-clauses
      (list 
      "AND (offerings.classificationcorrected = 25 "
      "OR offerings.classificationcorrected = 36)"))))
  
  (define non-supervisory-table
    (fch-table
     (append
      dept-clauses
      (list
      "AND (NOT (offerings.classificationcorrected = 25 "
      "OR offerings.classificationcorrected = 36))"))))
  
  (define datapoints 
    (for/list ([(k iff) (in-hash iff-table)]
               #:when (not (= iff 0)))
      
      (list k 
            (/ (hash-ref non-supervisory-table k 0)
               iff)
            (/ (hash-ref supervisory-table k 0)
               iff))))
  
  datapoints
  )

#;"WHERE id NOT IN (SELECT id FROM tenuretracks) "


(define (flatten-stats datapoints supervisory-conversion-frac)
  (for/list ([d datapoints])
    (+ (list-ref d 1)
       (* supervisory-conversion-frac
          (list-ref d 2)))))

(define (single-dept-graphs clauses filename-stem)
  (define datapoints
    (data-for-group clauses 
                    "id NOT IN (SELECT id FROM tenuretracks) "))
  
  #;(display
   (plot #;-file
         #:x-label "non-supervisory hours per week per iff"
         #:y-label "supervisory hours per week per iff"
         (points datapoints)
         #;(~a"/tmp/"filename-stem"-tt-faculty-contact-hours.pdf")))
  (newline)
  
  
  (define flattened (flatten-stats datapoints 0))
  
  (display
   (plot #;-file
        #:title "density of fch datapoints, assuming 0 hour per supervisory student"
        #:x-max 50.0
        #:x-label "contact hours per week per iff"
        #:y-label "density of points"
        
        (density flattened 0.25)
        #;(~a "/tmp/"filename-stem"-tt-faculty-classroom-contact-hours-density.pdf")))
  (newline)
  
  (define (median l)
    (list-ref (sort l <) (round (/ (length l) 2))))
  (define (mean l)
    (exact->inexact (/ (apply + l) (length l))))
  (display (~a"mean: "(mean flattened)"\n"))
  (display (~a"median: "(exact->inexact (median flattened))"\n"))
  )

(define (dept->dept-clauses dept)
  (list (~a "AND instructorstatuses.homedept = '"dept"' ")))

(define (run-for-dept dept)
  (display (~a "running report for dept: "dept"\n"))
  (single-dept-graphs 
   (dept->dept-clauses dept)
   dept))

(define depts
  (query-list
      conn
      "SELECT homedept FROM instructorstatuses GROUP BY homedept" ))

(define dept-data
  (apply 
   append
   (for/list ([d depts])
     (define dept-clauses (dept->dept-clauses d))
     (define non-tt-datapoints 
       (data-for-group dept-clauses
                       "id NOT IN (SELECT id FROM tenuretracks) "))
     (define non-tt-samples (flatten-stats non-tt-datapoints 1/3))
     (define tt-datapoints 
       (data-for-group dept-clauses
                       "id IN (SELECT id FROM tenuretracks) "))
     (define tt-samples (flatten-stats tt-datapoints 1/3))
     (list (list (~a d" TT") tt-samples)
           (list (~a d" non-TT") non-tt-samples))
     )))

(plot-file
 #:title "distribution of faculty contact hours, assuming 1/3 hr per supervisory student"
 #:y-label "department"
 #:x-label "contact hours per IFF"
 #:width 800
 #:height 500
 #:x-max 25.0
 (apply
  mix
  (for/list ([t dept-data]
             [pre-i (in-range (sub1 (length dept-data)) -1 -1)]
             [color-idx (in-naturals)])
    (define i (+ pre-i (floor (/ pre-i 2))))
    (define sorted-vals (sort (second t) <))
    (define mean (/ (apply + sorted-vals) (length sorted-vals)))
    (define q-size (/ (length sorted-vals) 4))
    (define lowest (first sorted-vals))
    (define highest (last sorted-vals))
    (define first-quartile (list-ref sorted-vals (round q-size)))
    (define second-quartile (list-ref sorted-vals (round (* 2 q-size))))
    (define third-quartile (list-ref sorted-vals (round (* 3 q-size))))
    (mix
     (rectangles
      #:label (first t)
      #:color color-idx
      (list (vector (ivl lowest first-quartile)
                    (ivl i i))
            (vector (ivl first-quartile second-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl second-quartile third-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl third-quartile highest)
                    (ivl i i))))
     (points
      #:color color-idx
      (list (vector (first sorted-vals) i)
            (vector mean i)
            (vector (last sorted-vals) i))))))
 "/tmp/faculty-contact-hours-grouped-by-dept.pdf")

(define evens (for/list ([i (in-range 0 (length dept-data) 2)]) (list-ref dept-data i)))
(define odds (for/list ([i (in-range 1 (length dept-data) 2)]) (list-ref dept-data i)))

(define rearranged
  (append evens odds))

(plot-file
 #:title "distribution of faculty contact hours, assuming 1/3 hr per supervisory student"
 #:y-label "department"
 #:x-label "contact hours per IFF"
 #:width 800
 #:height 500
 #:x-max 25.0
 (apply
  mix
  (for/list ([t rearranged]
             [pre-i (in-range (sub1 (length dept-data)) -1 -1)]
             [color-idx (in-naturals)])
    (define i (+ pre-i (floor (/ pre-i 9))))
    (define sorted-vals (sort (second t) <))
    (define mean (/ (apply + sorted-vals) (length sorted-vals)))
    (define q-size (/ (length sorted-vals) 4))
    (define lowest (first sorted-vals))
    (define highest (last sorted-vals))
    (define first-quartile (list-ref sorted-vals (round q-size)))
    (define second-quartile (list-ref sorted-vals (round (* 2 q-size))))
    (define third-quartile (list-ref sorted-vals (round (* 3 q-size))))
    (mix
     (rectangles
      #:label (first t)
      #:color color-idx
      (list (vector (ivl lowest first-quartile)
                    (ivl i i))
            (vector (ivl first-quartile second-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl second-quartile third-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl third-quartile highest)
                    (ivl i i))))
     (points
      #:color color-idx
      (list (vector (first sorted-vals) i)
            (vector mean i)
            (vector (last sorted-vals) i))))))
 "/tmp/faculty-contact-hours-grouped-by-classification.pdf")

(disconnect conn)) 