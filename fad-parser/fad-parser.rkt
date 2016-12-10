#lang racket

(require "pages-to-parsed.rkt")

(provide (all-defined-out))



;; return the first of a list of length 1.
(define (first-of-one l)
  (unless (= (length l) 1)
    (error 'first-of-one 
           "expected length of list 1, got: ~e"
           l))
  (first l))

(define (first-of-one-or-blank l)
  (match (length l)
    [0 ""]
    [1 (first l)]
    [other (error 'first-of-one 
           "expected length of list 1, got: ~e"
           l)]))

;; find an element in a list:
(define (list-find a l #:key [key (lambda (x) x)]
                   #:fail [fail (lambda (a l)
                                  (error 
                                   (~a "couldn't find "(~e a)
                                       " in list "(~e l))))])
  (let loop ([r l])
    (cond [(empty? r) (cond [(procedure? fail) (fail a l)]
                            [else fail])]
          [else (cond [(equal? a (key (first r))) (first r)]
                      [else (loop (rest r))])])))







#;(define grouped-fall (file->parsed "fad-2011-fall.txt"))

;; can we calculate wtus per class line correctly?

;; taken from "CSU Course Classification System"
(define lecture-classifications
  (for ([i (in-range 1 6)]) (number->string i)))
(define activity-classifications
  (for ([i (in-range 6 15)]) (number->string i)))
(define 3/4-lab-classifications
  '("15"))
(define lab-classifications
  '("16"))
(define triple-lab-classifications
  '("18"))
(define 3/2-lab-classifications
  '("19" "20" "21"))
(define supervisory-classifications
  '("25" "36"))


;; given a single course line, compute its direct wtus
(define (compute-wtus course)
  (define enrollment (string->number (col-ref 'enrollment course)))
  (define a-ccu (string->number (col-ref 'a-ccu course)))
  (define classification (col-ref 'classification course))
  (define team-teaching-frac (string->number (col-ref
                                              'team-teaching-frac
                                              course)))
  (* team-teaching-frac
     (case (string->number classification)
       [(1 2 3 4 5 6) a-ccu]
       [(7 8 9 10 11 12 13 14) (* a-ccu 1.3)]
       [(15) (* a-ccu 1.5)]
       [(16) (* a-ccu 2.0)]
       [(18) (* a-ccu 6.0)]
       [(19 20 21) (* a-ccu 3.0)]
       [(25) (* enrollment 1/2)]
       [(36) (* enrollment 1/3)])))

;;given the courses in a group, check the wtu sum
(define (wtu-sum-check courses)
  (define classifications
    (list->set (map (lambda (x) (col-ref 'classification x))
                    courses)))
  (define stated-wtu-sum
    (for/sum ([course courses]) 
      (string->number/0 (col-ref 'direct-wtu course))))
  (when (and 
         (not (set-empty? (set-intersect 
                           (list->set supervisory-classifications)
                           classifications)))
         (< 1 (length courses)))
    (error 'wtu-sum-check "~a"
           (~a "expected only non-supervisory classes in "
               "a group, given: "(~e courses))))
  (for ([course courses])
    (define diff (- (compute-wtus course) stated-wtu-sum))
    (when (not (small? diff 0.050001))
      (display (~a "difference: "diff"\n"
                   (~s courses)"\n")))))

#;'((("ENVE" "0581" 0.45) ("ENGR" "0581" 0.55))
  (("BMED" "0550" 0.025) ("BMED" "0550" 0.975)))

(define (wtu-balance courses)
  (define stated-wtu-sum
    (for/sum ([course courses]) 
      (string->number/0 (col-ref 'direct-wtu course))))
  (for/list ([c courses])
    (list (col-ref 'dept c)
          (col-ref 'course-num c)
          (/ (/ (string->number/0 (col-ref 'direct-wtu c))
                stated-wtu-sum)
             (string->number/0 (col-ref 'team-teaching-frac c))))))


(define (only-courses c)
  (filter (lambda (x) (equal? (col-ref 'kind x) 'class)) c))

#;(for ([instr instructors-fall])
  (define course-table
    (group-by (only-courses (instructor-courses instr))
              (lambda (x) (col-ref 'group-code x))))
  (for ([(group-code courses) (in-hash course-table)])
    (cond [(equal? group-code "")
           (map wtu-sum-check (map list courses))]
          [else
           (wtu-sum-check courses)])))

;; find grouped courses, and their WTU split:
#;(apply
 append
 (for/list ([instr instructors-fall])
  (define course-table
    (group-by (only-courses (instructor-courses instr))
              (lambda (x) (col-ref 'group-code x))))
  (apply
   append
   (for/list ([(group-code courses) (in-hash course-table)])
    (cond [(equal? group-code "")
           empty]
          [else
           (list (wtu-balance courses))])))))

(define senior-project-classes
  '(("AERO" "0463" "16")
    ("AERO" "0464" "16")
    ("CSC" "0491" "16")
    ("CSC" "0492" "16")
    ("CPE" "0461" "16")
    ("CPE" "0462" "16")
    ("EE" "0463" "16")
    ("EE" "0464" "16")))

;; correct for labwashing accounting error by recomputing WTUs for senior projects:
;; instructor->instructor
(define (un-labwash instr)
  (define-values (special-courses class-courses)
    (partition special? (instructor-courses instr)))
  (define new-courses
    (for/list ([course class-courses])
      (cond 
        [(and (has-group? course)
              (is-senior-project? course))
         (error 'un-labwash
                "un-labwash can't deal with senior project courses"
                " that also have group codes.")]
        [(is-senior-project? course)
         (assoc-replace
          course
          `((classification "36")
            (direct-wtu ,(number->string
                          (* (string->number
                              (col-ref 'enrollment course))
                             (string->number
                              (col-ref 'team-teaching-frac course))
                             1/3)))))]
        [else 
         course])))
  (define new-all-courses (append new-courses special-courses))
  (define new-direct-wtu
    (apply + (map (lambda (x) 
                    (string->number/0 (col-ref 'direct-wtu x)))
                  new-all-courses)))
  (define new-total-wtu
    (+ new-direct-wtu
       (string->number/0
        (col-ref 'indirect-wtu (instructor-summary instr)))))
  (instructor (instructor-header instr)
              new-all-courses
              (assoc-replace (instructor-summary instr)
                             `((direct-wtu ,(number->string
                                             new-direct-wtu))
                               (total-wtu ,(number->string
                                            new-total-wtu))))))



(define (has-group? course)
  (not (equal? (col-ref 'group-code course) "")))

(define (is-senior-project? course)
  (member (list (col-ref 'dept course)
                (col-ref 'course-num course)
                (col-ref 'classification course))
          senior-project-classes))

(define (assoc-replace course replacements)
  (define names-to-replace (map first replacements))
  (define existing-names (map first course))
  (unless (subset (list->set names-to-replace)
                  (list->set existing-names))
    (error 'assoc-replace "~a"
           (~a "new names are not a subset of existing ones: "
               names-to-replace", "existing-names)))
  (append
   (filter (lambda (field) (not (member (first field) names-to-replace)))
           course)
   replacements))

(define (subset a b)
  (equal? a (set-intersect a b)))



(define (find-instructor name instructors)
  (list-find name instructors #:key instructor-name))


#|

(define grouped-winter (read-and-group "fad-2012-winter.txt"))
(define instructors-winter (grouped->single-instructors grouped-winter))
(sanity-check/totals grouped-winter instructors-winter)


(define grouped-spring (read-and-group "fad-2012-spring.txt"))
(define instructors-spring (grouped->single-instructors grouped-spring))
(sanity-check/totals grouped-spring instructors-spring)

(define all-instructors (append instructors-fall 
                                instructors-winter
                                instructors-spring))

;; taken from "CSU Course Classification System"
(define lecture-classifications
  (for ([i (in-range 1 6)]) (number->string i)))
(define activity-classifications
  (for ([i (in-range 6 15)]) (number->string i)))
(define 3/4-lab-classifications
  '("15"))
(define lab-classifications
  '("16"))
(define triple-lab-classifications
  '("18"))
(define 3/2-lab-classifications
  '("19" "20" "21"))
(define supervision-classifications
  '("25" "36"))

;; try to find mis-classified classes

(define senior-project-classes
  '(("AERO" "0463" "16")
    ("AERO" "0464" "16")
    ("CSC" "0491" "16")
    ("CSC" "0492" "16")
    ("CPE" "0461" "16")
    ("CPE" "0462" "16")
    ("EE" "0463" "16")
    ("EE" "0464" "16")))

(define (course-dn course)
  (list (col-ref 'dept course) (col-ref 'course-num course)))
(define (course-dnc course)
  (list (col-ref 'dept course)
        (col-ref 'course-num course)
        (col-ref 'classification course)))

(define all-courses
  (filter (lambda (c) (not (equal? (col-ref 'kind c) 'special)))
          (apply append (map instructor-courses all-instructors))))


(define grouped-by-dept/num
  (group-by all-courses course-dnc))

(define (mean l)
  (/ (apply + l) (length l)))

(define dnc-enrollments
  (for/list ([(key val) (in-hash grouped-by-dept/num)])
    (define enrollments
      (map string->number/0 
           (for/list ([course val]) (col-ref 'enrollment course))))
    (list key
          (mean enrollments)
          enrollments)))

(define (dn-< a b)
  (or (string<? (first a) (first b))
      (and (string=? (first a) (first b))
           (< (string->number (second a))
              (string->number (second b))))))

(define (not-supervisory? enrollment)
  (not (member (third (first enrollment))
               supervision-classifications)))

;; looks like we've got most of 'em....
#;(take 
 (sort (filter
        (lambda (x) (not (member (first x)
                                 senior-project-classes)))
        (filter not-supervisory? dnc-enrollments))
       < #:key second)
 100)

#;(sort (take (sort dn-enrollments < #:key second) 100)
      dn-<
      #:key first)
#;(take (sort dn-enrollments > #:key second) 10)


#;(for ([dn all-course-dept/nums])
  (define )
  (for/list ([c dn])))


;; total wtus by department
(for ([dept (parsed-depts grouped-fall)])
  (define computed-sum
    (for/sum ([instructor (dept->instructors dept)]
              #:when (home-record instructor))
     (string->number/0
      (col-ref 'total-wtu (instructor-summary instructor)))))
  (define given-sum
    (summary-column-ref 
    'total-wtu
    (assoc "TOTAL" (dept-pages-summary dept))))
  (unless (small? (- given-sum computed-sum) (* 0.05 20))
    (error (~a "computed wtu total "computed-sum
               " differs substantially from given"
               " wtu total "given-sum" for department "
               (dept-pages-dept dept)".")))
  )




(check-equal? (group-by '((a 3) (b 4) (a 9) (d 12)) first)
              (hash 'a '((a 9) (a 3))
                    'b '((b 4)) 'd '((d 12))))|#