#lang racket

;; I think all of this code is out of date.
(module+ main
(require db)

(define conn
   (mysql-connect #:user "clements"
                 #:database "fad"))

;; DIRECT WTU QUERY:

(define (dept-dwtus dept qtr)
  (define extra-dwtus (vector-ref (first (dept-specialcredits dept qtr))
                                  0))
  (+ extra-dwtus
     (query-value
   conn
   (string-append
    "SELECT SUM(dwtucorrected) FROM instructors, instructorstatuses, offerseqs WHERE "
    "(instructors.id)=(instructorstatuses.id) AND "
    "(instructors.id)=(offerseqs.instructor) AND "
    "(offerseqs.qtr)=(instructorstatuses.qtr) AND "
    "offerseqs.qtr = ? AND instructorstatuses.homedept = ?")
   qtr
   dept)))

(define (college-dwtus qtr)
  (define extra-dwtus (vector-ref (first (college-specialcredits qtr))
                                  0))
  (+ extra-dwtus
     (query-value
      conn
      (string-append
       "SELECT SUM(dwtucorrected) FROM instructors, instructorstatuses, offerseqs WHERE "
       "(instructors.id)=(instructorstatuses.id) AND "
       "(instructors.id)=(offerseqs.instructor) AND "
       "(offerseqs.qtr)=(instructorstatuses.qtr) AND "
       "offerseqs.qtr = ?")
      qtr)))

(define (dept-specialcredits dept qtr)
  (query-rows
   conn
   (string-append
    "SELECT SUM(dwtu), SUM(iwtu) "
    "FROM instructors, instructorstatuses, specialcredits "
    "WHERE instructors.id = instructorstatuses.id "
    "AND instructors.id = specialcredits.instructor "
    "AND specialcredits.qtr = instructorstatuses.qtr "
    "AND specialcredits.qtr = ? "
    "AND instructorstatuses.homedept = ?")
   qtr
   dept))

(define (college-specialcredits qtr)
  (query-rows
   conn
   (string-append
    "SELECT SUM(dwtu), SUM(iwtu) "
    "FROM instructors, instructorstatuses, specialcredits "
    "WHERE instructors.id = instructorstatuses.id "
    "AND instructors.id = specialcredits.instructor "
    "AND specialcredits.qtr = instructorstatuses.qtr "
    "AND specialcredits.qtr = ? ")
   qtr))

;; ENROLLMENT QUERY:

(define qtrs
  '(2084 2088 2092 2094 2098 2102 2104 2108 2112 2114
  2118 2122 2124 2128))

(define (enrollment-in-qtr num qtr)
  (query-value 
   conn 
   "SELECT SUM(enrollment) FROM offerings WHERE subject = 'CPE' AND num = ? AND qtr = ?"
   num qtr))

#;(for/list ([p qtrs]
           [q (rest qtrs)]
           [r (rest (rest qtrs))]
           [s (rest (rest (rest qtrs)))])
  (define 123-students (enrollment-in-qtr "0123" p))
  (define 101-students (enrollment-in-qtr "0101" q))
  (define 102-students (enrollment-in-qtr "0102" r))
  (define 103-students (enrollment-in-qtr "0103" s))
  (list  p 123-students 101-students 102-students 103-students)
  )


;; distribution of WTU sums
(define 
  a
  (sort 
 (map
  (lambda (e)
    (vector-ref e 0))
  (apply
   append
   (for/list ([qtr qtrs])
     (query-rows
      conn
      (string-append
       "SELECT SUM(dwtucorrected)"
       "FROM instructors, offerseqs "
       "WHERE (instructors.id)=(offerseqs.instructor) "
       "AND offerseqs.qtr = ? "
       "GROUP BY instructors.id")
      qtr))))
 <))

(require plot)


(define bin-size 2000)
(define min-a (apply min a))
(define max-a (apply max a))
(define grouped
  (for/fold ([ht (hash)]) ([datum a])
    (define rounded (* bin-size (round (/ datum bin-size))))
    (hash-set ht rounded (add1 (hash-ref ht rounded 0)))))

(define vecs
  (sort (for/list ([(k v)(in-hash grouped)])
          (vector k v))
        <
        #:key (lambda (v) (vector-ref v 0))))


#;(plot-file (discrete-histogram vecs)
           "/tmp/dwtu-distribution.pdf")

(define point-names
  (query-rows
   conn
   "SELECT id,qtr,homedept FROM instructorstatuses"))

(define (instructor-offerseqs name qtr)
  (query-rows
   conn
   (~a
    "SELECT classificationcorrected, dwtucorrected FROM offerseqs, offerings "
    "WHERE (offerseqs.qtr,offerseqs.subject,offerseqs.num,offerseqs.section) "
    "= (offerings.qtr,offerings.subject,offerings.num,offerings.section) "
    "AND offerseqs.instructor = ? "
    "AND offerseqs.qtr = ?")
   name
   qtr))

(define (instructor-fracs rows)
  (define ht
    (for/fold ([ht (hash)]) ([r rows])
      (hash-set ht (vector-ref r 0)
                (+ (vector-ref r 1)
                   (hash-ref ht (vector-ref r 0) 0)))))
  (define sum (for/sum ([(k v) (in-hash ht)]) v))
  (define unknown-sum (for/sum ([(k v) (in-hash ht)]
                                #:when (not (member k '(1 2 4 5 7 13 16 36 25))))
                        v))
  (cond [(= sum 0) #f]
        [else 
         (define lecture-frac (/ (+ (hash-ref ht 1 0)
                                    (hash-ref ht 2 0)
                                    (hash-ref ht 4 0)
                                    (hash-ref ht 5 0)) sum))
         (define lab-frac (/ (+ (hash-ref ht 16 0)
                                (hash-ref ht 13 0)
                                (hash-ref ht 7 0)) sum))
         (define unknown-frac (/ unknown-sum sum))
         (when (< 0 unknown-frac)
           (error (~a "problem for these rows: "rows)))
         (vector (+ (/ (random) 50) lecture-frac)
                 (+ (/ (random) 50) lab-frac))]))

(define dept-names
  (map second
       '(["AERO ENG" "AERO"]
         ["ALL SCHOOL" "ALLSCHOOL"]
         ["CIVIL/ENV ENG" "CEENVE"]
         ["COMPUTER SCIENCE" "CSC"]
         ["BIOMEDICAL ENGINEERING" "BMGE"]
         ["ELECTRICAL ENGINEERING" "EE"]
         ["IND ENG" "IME"]
         ["MECHANICAL ENG" "ME"]
         ["WELDING AND METALLURGICAL ENGINEERING" "MATE"])))


(define (color-lookup dept)
  (length (member dept dept-names)))

(define (generate-for-dept dept-name)
  (define data
    (for/list ([info point-names]
               [i (in-naturals)]
               #:when (equal? (vector-ref info 2) dept-name))
      (when (= (modulo i 200) 0)
        (display (~a "on record: "i"\n")))
      (define fracs 
        (instructor-fracs (instructor-offerseqs (vector-ref info 0) 
                                                (vector-ref info 1))))
      (cond [fracs
             (points
              #:color (color-lookup (vector-ref info 2))
              (list 
               fracs))]
            [else
             (points (list))])
      ))
  
  
  (plot-file
   #:title (~a dept-name" lab-lecture-supervision balance")
   #:x-label "fraction of Direct WTUs from Lecture"
   #:y-label "fraction of Direct WTUs from Lab"

   (apply
    mix
    data)
   (~a "/tmp/"dept-name"-balance.pdf")))

#;(map generate-for-dept dept-names)

#;(length point-names)

(disconnect conn))