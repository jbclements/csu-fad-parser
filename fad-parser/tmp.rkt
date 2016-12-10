#lang racket

(require fad-parser/all-data
         fad-parser/fad-to-pages
         fad-parser/pages-to-parsed-tr
         parser-tools/lex
         explorer
         plot
         racket/fasl)

#;(define parsed-qtr-atoms
  (call-with-input-file "/tmp/zz1"
    fasl->s-exp))

(define FAD-DIRECTORY "/Users/clements/clements/datasets/FAD/")



(define (categorize-values value-set)
  (cond [(andmap (λ (s) (equal? s "")) value-set)
         'all-blank]
        [(andmap (λ (s) (equal? s "0.0")) value-set)
         'all-zero]
        [(equal? value-set `("")) 'one-blank]
        [(equal? value-set `("0.0")) 'one-zero]
        [(= (length value-set) 1) 'one-nonblank-nonzero]
        [(andmap (λ (s) (equal? s "")) (rest value-set))
         'blank-after-first]
        [(andmap (λ (s) (equal? s "0.0")) (rest value-set))
         'zero-after-first]
        [(= 1 (length (filter (λ (s) (not (equal? s ""))) value-set)))
         'one-non-blank]
        [(= 1 (length (filter (λ (s) (not (equal? s "0.0"))) value-set)))
         'one-non-zero]
        [(= 1 (length (remove-duplicates value-set)))
         'all-the-same]
        [else 'heterogeneous]))

(define descriptors
  (map third
       '(((0 0) alpha dept)
         ((12 7) course-num course-num)
         ((22 12) nums section)
         ((26 19) nums discipline)
         ((31 25) alphanum level)
         ((34 29) nums enrollment)
         ((38 33) seq-num sequence)
         ((43 37) nums classification)
         ((46 40) decimal a-ccu)
         ((51 46) alpha days)
         ((58 52) nums time-start)
         ((63 57) nums time-stop)
         ((68 62) decimal tba-hours)
         ((75 68) alphanum facility)
         ((79 73) alphanum space)
         ((86 79) alphanum facility-type)
         ((90 84) nums group-code)
         ((95 89) decimal team-teaching-frac)
         ((101 95) decimal scu)
         ((109 102) decimal faculty-contact-hours)
         ((114 109) decimal direct-wtu)
         ((121 116) decimal indirect-wtu)
         ((127 123) decimal total-wtu))))



(define COLUMN 'a-ccu)

(define (hist l)
  (map (λ (g) (list (first g) (length g)))
       (group-by (λ (x) x) l)))

;; by class
(define (instr-column-hist column)
  (for/list ([pq (in-list parsed-qtr-atoms)]
           [qtr (in-list qtr-nums)])
  (define multi-atom-sections
    (filter
     (λ (c) (< 1 (length c)))
     pq))
  (list
   qtr
   (hist
    (for/list ([c (in-list multi-atom-sections)])
      (categorize-values (map (λ (atom) (col-ref column atom))
                              c)))))))

;; regrouped by instructor
(define (column-hist column)
  (for/list ([pq (in-list parsed-qtr-atoms)]
           [qtr (in-list qtr-nums)])
  (define multi-atom-instructor-sections
    (apply
     append
     (map
      (λ (g) (group-by (λ (a) (col-ref 'instructor a)) g))
      (filter
       (λ (c) (< 1 (length c)))
       pq))))
  (list
   qtr
   (hist
    (for/list ([c (in-list multi-atom-instructor-sections)])
      (categorize-values (map (λ (atom) (col-ref column atom))
                              c)))))))

(instr-column-hist COLUMN)
(column-hist COLUMN)

(define INDEX 17)

(define multi-atom-sections
    (filter
     (λ (c) (< 1 (length c)))
     (list-ref parsed-qtr-atoms INDEX)))

(explore multi-atom-sections)
(explore
 (for/list ([c (in-list multi-atom-sections)])
   (list (first c)
         (categorize-values (map (λ (atom) (col-ref COLUMN atom))
                               c)))))

(define multi-atom-instructor-sections
  (apply
   append
   (map
    (λ (g) (group-by (λ (a) (col-ref 'instructor a)) g))
    (filter
     (λ (c) (< 1 (length c)))
     (list-ref parsed-qtr-atoms INDEX)))))
(list-ref qtr-nums INDEX)
(hist
 (for/list ([c (in-list multi-atom-instructor-sections)])
   (map (λ (atom) (col-ref COLUMN atom))
        c)))

(explore multi-atom-instructor-sections)
(explore
 (for/list ([c (in-list multi-atom-instructor-sections)])
   (list (col-ref 'instructor (first c))
         (categorize-values (map (λ (atom) (col-ref COLUMN atom))
                               c)))))



#;(explore (first parsed-qtrs))


#;(define all-course-groups
  (for/list ([parsed (in-list z)])
    (filter
     (λ (c) (< 1 (length c)))
     (Parsed-atoms parsed))))



#;(explore
 (for/list ([parsed (in-list z)])
  (filter
  (λ (i) (> (abs (- 1.0 (second i))) 1e-4))
 (apply
  append
 (map (λ (g)
        (map
         (λ (sg) (list (first (first sg))
                       (apply +
                              (map (compose string->number/0 third) sg))))
        (group-by
         first
        (map (λ (c)
               (list (col-ref 'instructor c)
                     (col-ref 'sequence c)
                     (col-ref 'team-teaching-frac c))) g))))
 (filter
  (λ (c) (< 1 (length c)))
  (Parsed-atoms parsed)))))))