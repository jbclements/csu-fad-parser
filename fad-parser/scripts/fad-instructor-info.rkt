#lang racket

;; It looks like recent FAD reports include the EMPLID, which is a much better notion
;; of person identity than the partially-XXX-ed out SSN.

;; let's start by figuring out the multi-mappings in the existing sets

(require "../one-quarter-data.rkt"
         "../parsed-data-defn.rkt"
         tabular-asa
         explorer)

;; looks like the old format is a bit different.
(define qtrs (filter (λ (qtr) (< 2142 qtr)) (qtrs-available)))

(define all-fad-instructors
  (table-read/sequence
   (apply
    append
    (for/list ([qtr (sort qtrs >)])
      (printf "parsing qtr: ~v\n" qtr)
      (apply
       append
       (map (λ (d) (map
                    (λ (i)
                      (list qtr
                            (Dept-name d)
                            (Instructor-name i)
                            (Instructor-id i)
                            (Instructor-other-id i)))
                    (Dept-instructors d)))
            (Parsed-depts (qtr->parsed qtr))))))
   
   '(qtr dept name id emplid)))

(define a
  (remove-duplicates
   (sequence->list
    (table-column 
     (table-filter all-fad-instructors (λ (emplid) (equal? emplid "")) '(emplid))
     'qtr))))
(define b
  (remove-duplicates
   (sequence->list
    (table-column 
     (table-filter all-fad-instructors (λ (emplid) (not (equal? emplid ""))) '(emplid))
     'qtr))))

;; looks like quarters in 2168 and later include emplid, those before do not.

;; looks like there are two rows with the same name and emplid but different ssnxs:
#|'((((name "G E SERNA VALENZUELA"))            qtr      dept          id            emplid
    198   2244   IND ENG   XXXXX6571   000000017370569
   1346   2232   IND ENG   XXXXX0002   000000017370569

[2 rows x 4 cols]
) (((name "P M BECKER"))            qtr             dept          id            emplid
    209   2244   MECHANICAL ENG   XXXXX0005   000000015333964
   6013   2178   MECHANICAL ENG   XXXXX0013   000000015333964

[2 rows x 4 cols]
))
|#

;; oh dear, there are *lots* of rows with the same emplid and different names. 13, to
;; be precise. They all appear to represent people who have changed their names.

;; this is going to mean some database redesign....

;; okay, but to the best of my knowledge we don't yet have any name collisions,
;; so our existing notion of id is probably fine, it's just that some remapping
;; is required.

(define with-emplids
  (table-filter all-fad-instructors (λ (qtr) (< 2164 qtr)) '(qtr)))

(define z2
  (table-distinct with-emplids '(name id emplid)))

(define n1
  (filter
   (λ (l) (< 1 (table-length (second l))))
   (sequence->list
    (sequence-map
     (λ (a b) (list a b))
     (table-groupby z2 '(name))))))

(define n2
  (filter
   (λ (l) (< 1 (table-length (second l))))
   (sequence->list
    (sequence-map
     (λ (a b) (list a b))
     (table-groupby z2 '(emplid))))))

(define n3
  (filter
   (λ (l) (< 1 (table-length (second l))))
   (sequence->list
    (sequence-map
     (λ (a b) (list a b))
     (table-groupby z2 '(id))))))

(define n3wide
  (filter
   (λ (l) (< 1 (table-length (second l))))
   (sequence->list
    (sequence-map
     (λ (a b) (list a b))
     (table-groupby (table-distinct all-fad-instructors '(name id emplid)) '(id))))))

(define (table-to-file t f)
  (call-with-output-file f
    (λ (port)
      (pretty-write
       (list->vector
        (map list->vector
             (sequence->list (table-rows t))))
       port))))

(table-to-file (table-cut all-fad-instructors '(qtr name id emplid))
               "/tmp/instructor-data.rktd")


(define ssn-groups
(sequence->list
 (sequence-map
  (λ (a b) (list a (remove-duplicates (sequence->list (table-column b 'id)))))
  (table-groupby (table-distinct all-fad-instructors '(name id emplid)) '(name)))))

(filter (λ (g) (< 1 (length (second g))))
        ssn-groups)

#;(call-with-output-file "/tmp/"
(table-write/csv
 (table-cut all-fad-instructors '(qtr name id emplid))))

#;(
#;(define infos
  (remove-duplicates
  (apply
   append
   (map
   second
   (apply
   append
   (map second all-fad-instructors))))))

;; 392 records have empty emplids

(count (λ (t) (equal? (third t) "")) infos)

(map (λ (qr)
       (list (first qr)
             (count (λ (ir) (equal? (third ir) ""))
                    (apply append map second (second qr)))))
     all-fad-instructors)

(explore
 (filter
 (λ (g) (< 1 (length g)))
 (group-by first infos))))