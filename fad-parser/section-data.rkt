#lang racket

(require db
         plot)

(define conn
  (mysql-connect #:user "clements"
                 #:database "fad"
                 #:password "aoeuidht"
                 #:port 11306
                 ))

;; CLASSES TO OMIT
(define omits
  (list
   #;"AND NOT (subject='CPE' AND num='0100') "))

;; 2225
(define CPE-enrollments
  (query-list conn
              (apply
               ~a "SELECT enrollment FROM offerings WHERE (subject='CPE' "
               "OR subject='CSC') "
               omits)))

(printf "got ~v rows\n" (length CPE-enrollments))

(sort
 (query-rows 
  conn
  (apply
   ~a 
   "SELECT subject,num,section,qtr,enrollment FROM offerings WHERE (subject='CPE' "
   "OR subject='CSC') AND enrollment > 49 "
   omits))
 <
 #:key
 (lambda (a) (vector-ref a 4)))

(plot (density CPE-enrollments))

;; listof students instances
(define enrollments-hash
  (for/fold ([ht (hash)])
            ([d CPE-enrollments])
    (hash-set ht d (add1 (hash-ref ht d 0)))))

(define max-enrollment (apply max CPE-enrollments))
(define occurrences
  (for/list ([i (in-range 0 (add1 max-enrollment) 5)])
    (vector i 
            (for/sum ([j (in-range i (+ i 5))])
              (hash-ref enrollments-hash j 0)))))

(plot-file
 #:title "CSC and CPE enrollments (selected classes), 2008 fall - 2012 spring"
 (discrete-histogram
  occurrences)
 #:x-label "# of students"
 #:y-label "# of sections"
 "/tmp/sections-of-students.pdf")

(plot-file
 #:title "CSC and CPE enrollments (selected classes), 2008 fall - 2012 spring"
 (discrete-histogram
  occurrences)
 #:x-label "# of students"
 #:y-label "# of sections"
 "/tmp/sections-of-students.pdf")

(define gt-35
  (filter (lambda (p) (< 34 (vector-ref p 0))) occurrences))

(plot-file
 #:title "CSC and CPE enrollments > 35 (selected classes), 2008 fall - 2012 spring"
 (discrete-histogram
  gt-35)
 #:x-label "# of students"
 #:y-label "# of sections"
 "/tmp/sections-of-students-gt-35.pdf")

#;(plot-file
 (discrete-histogram
  (map (lambda (p) (vector (car p) (cdr p))) sorted-list))
 "/tmp/sections-of-students.pdf")


