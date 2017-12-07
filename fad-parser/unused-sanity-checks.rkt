#lang racket

;; this file used to be the second stage of parsing.

;; now it has a bunch of unused sanity checks. I
;; think that most of them can be reformulated as
;; checks on the database, which would be nicer
;; anyway.

;; EDIT: or maybe they should be checks on the parsed struct files.

(require "divide-columns.rkt"
         "pages-to-parsed-tr.rkt"
         "fad-to-pages.rkt"
         explorer
         db)

;; here's a check...

(define conn
  (postgresql-connect ))

(define
  sums
  (query-rows
   conn
   (~a
    "SELECT o.subject,o.num,o.qtr,o.section,
            o.enrollment,o.accu,SUM(f.scu) FROM "
    " offerings o INNER JOIN offerfacs f "
    "  ON o.subject = f.subject AND o.num = f.num "
    "  AND o.qtr = f.qtr AND o.section = f.section "
    " WHERE (o.classification IS NOT NULL) "
    " GROUP BY o.subject,o.num,o.qtr,o.section "
    " ORDER BY o.qtr;")))

(require explorer)
(define bads
  (filter (λ (r)
            (> (abs
                (- (* (vector-ref r 4)
                      (vector-ref r 5))
                   (vector-ref r 6)))
               
               400))
          sums))
(length bads)
(explore
 bads)

(define assoc-list? (listof (list/c symbol? (or/c symbol? string? (listof string?)))))
(define summary-line? (list/c string? (listof string?)))

(define format/c (symbols 'pre-2144 'post-2142))



;; parse a file
#;(define (file->parsed file format)
  (fad-pages->parsed (file->fad-pages file format) format))

;; parse a set of fad-pages
#;(define (fad-pages->parsed fad-pages format)
  (define result (parse-pages fad-pages format))
    ;; here are the checks we used to have:
  (check-no-duplicated-names
   (filter instructor-home?
           (apply append
                  (map dept-instructors 
                       (parsed-depts result)))))
  (sanity-check/sums result)
  (sanity-check/college-sum result)
  result)



;; I want a contract to get a handle on this...
(define parsed-class-field/c
  (or/c (list/c 'kind (or/c 'class 'special))
        (list/c 'dept string?)
        (list/c 'course-num string?)
        (list/c 'section string?)
        (list/c 'discipline string?)
        (list/c 'level string?)
        (list/c 'enrollment string?)
        (list/c 'group-code string?)
        (list/c 'team-teaching-frac (or/c string? (listof string?)))
        (list/c 'scu (or/c string? (listof string?)))
        (list/c 'indirect-wtu (or/c string? (listof string?)))
        (list/c 'total-wtu (or/c string? (listof string?)))
        (list/c 'direct-wtu (or/c string? (listof string?)))
        (list/c 'faculty-contact-hours (or/c string? (listof string?)))
        (list/c 'sequence (listof string?))
        (list/c 'time-stop (listof string?))
        (list/c 'space (listof string?))
        (list/c 'days (listof string?))
        (list/c 'facility (listof string?))
        (list/c 'facility-type (listof string?))
        (list/c 'time-start (listof string?))
        (list/c 'a-ccu (listof string?))
        (list/c 'tba-hours (listof string?))
        (list/c 'classification (listof string?))
        (list/c 'special string?)))

(define parsed-class/c
  (listof parsed-class-field/c))

;; a class-line is a mapping from names to strings;

;; a class is a mapping from names to strings *or* lists-of-strings


;; ensure just one non-blank, return it
(define (max-once l)
  (match (filter (λ (x) (not (string=? x ""))) l)
    [(list v) v]
    [other (raise-argument-error 'max-once
                                 "list containing exactly one non-blank value"
                                 0 l)]))





;; filter out blank lines
(define (noblanks los)
  (filter (lambda (s) (not (equal? s ""))) los))





;;;;;;;
;;
;; SANITY CHECKS
;;
;;;;;;;

;; do all the wtus add up?

#;(define (sanity-check/sums parsed)
  ;; do the instructor sums add up?
  ;; for pre-2144, things are weird, and are checked
  ;; in home-dept.
    ;;
    ;; er... except that they're not, any more. Um.
  (when (eq? format 'post-2142)
    (for* ([d (in-list (parsed-depts parsed))]
           [i (in-list (dept-instructors d))])
      (for ([col (in-list '(scu faculty-contact-hours
                                direct-wtu indirect-wtu))])
        (define computed-total
          (for/sum ([course (in-list (instructor-courses i))])
            (string->number/0 (col-ref/g col course))))
        (define stated-total (string->number/0
                              (col-ref col (instructor-summary i))))
        (when (not 
               (small? (- computed-total stated-total) 
                       (* (length (instructor-courses i)) 0.1)))
          (error (~a "too great a difference between computed total "
                     computed-total" and stated total "
                     stated-total" for instructor "
                     (~s (instructor-name i))))))))
  ;; do the department sums add up?
  (for ([d (parsed-depts parsed)])
    (define computed-wtu-total
      (for/sum ([i (dept-instructors d)])
        (string->number/0 
         (col-ref 'total-wtu
                  (instructor-summary i)))))
    (define computed-scu-total
      (for/sum ([i (dept-instructors d)])
        (string->number/0 
         (col-ref 'scu
                  (instructor-summary i)))))
    (define stated-wtu-total
      (summary-column-ref
       'total-wtu
       (assoc "TOTAL" (dept-summary d))))
    (define stated-scu-total
      (summary-column-ref
       'total-scu
       (assoc "TOTAL" (dept-summary d))))
    ;; these can be off by 0.05 per faculty course....
    (when (not 
           (small? (- computed-wtu-total stated-wtu-total) 
                  (* (length (dept-instructors d)) 0.1)))
      (error (~a "too great a difference between computed wtu total "
                 computed-wtu-total" and stated wtu total "
                 stated-wtu-total" in department "
                 (~s (dept-name d)))))
    (when (not 
           (small? (- computed-scu-total stated-scu-total) 
                  (* (length (dept-instructors d)) 0.1)))
      (error (~a "too great a difference between computed scu total "
                 computed-scu-total" and stated scu total "
                 stated-scu-total" in department "
                 (~s (dept-name d))))))
  )

#;(define (sanity-check/college-sum parsed)
  (define all-instructors
    (apply append (map dept-instructors (parsed-depts parsed))))
  (define computed-wtu-total
    (for/sum ([i all-instructors])
      (string->number/0 (col-ref 'total-wtu (instructor-summary i)))))
  (define computed-scu-total
    (for/sum ([i all-instructors])
      (string->number/0 (col-ref 'scu (instructor-summary i)))))
  (define stated-wtu-total
    (summary-column-ref
     'total-wtu
     (assoc "TOTAL" (parsed-college-summary parsed))))
  (define stated-scu-total
    (summary-column-ref
     'total-scu
     (assoc "TOTAL" (parsed-college-summary parsed))))
  ;; these can be off by 0.05 per faculty course....
  (when (not 
         (small? (- computed-wtu-total stated-wtu-total) 
                (* (length all-instructors) 0.1)))
    (error (~a "too great a difference between computed wtu total "
               computed-wtu-total" and stated wtu total "
               stated-wtu-total" for college.")))
  (when (not 
         (small? (- computed-scu-total stated-scu-total) 
                (* (length all-instructors) 0.1)))
    (error (~a "too great a difference between computed scu total "
               computed-scu-total" and stated scu total "
               stated-scu-total" for college."))))


;; built a hash table using a given key:
(define (my-group-by l keyfun)
  (for/fold ([ht (hash)]) ([elt l])
    (hash-set ht (keyfun elt) (cons elt (hash-ref ht (keyfun elt) empty)))))

#;(define (instructor-name i)
  (col-ref 'name (instructor-header i)))

;; try it out: 
;(define p (file->parsed "fad-2011-spring.txt"))


;; waiting to hear from CSU on this....
#;(define (find-differing-instructor-pairs fad-pages)
  (define instructor-map
    (group-by
     (apply 
      append
      (for/list ([dept-pages (fad-pages-depts fad-pages)])
        (map 
         parse-instructor
         (dept-lines-parser
          (lines->token-feed
           (dept-pages-detail dept-pages))))))
     instructor-name))
  ;; find different sets of courses.
  #;(for ([(name entries) (in-hash instructor-map)])
    (define course-lists (remove-duplicates
                          (map instructor-courses entries)))
    (unless (= (length course-lists) 1)
      (display 
       (~a "instructor "name" has different sets of courses: "
           (~s course-lists)"\n\n"))))
  ;; find specials that occur more than once
  (for ([(name entries) (in-hash instructor-map)])
    (when (< 1 (length entries))
      (define specialses
        (for/list ([entry entries]) 
          (filter special? entry)));; RIGHT HERE (... as of when?)
      (unless (= (length (apply append )))))))

#;(find-differing-instructor-pairs
 (file->fad-pages "fad-2008-fall.txt"))





