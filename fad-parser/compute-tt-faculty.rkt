#lang racket

;; FIGURING OUT WHICH FACULTY ARE TENURE-TRACK

(require db
         "make-connection.rkt"
         (only-in "pages-to-parsed.rkt" group-by))

(define conn
   (mysql-connect #:user "clements"
                 #:database "fad"
                 #:password "aoeuidht"))

(define rows
  (map (lambda (v) (vector-ref v 0))
       (remove-duplicates
        (query-rows
         conn
         (string-append
          "SELECT instructors.id FROM instructors, instructorstatuses "
          "WHERE instructors.id = instructorstatuses.id "
          "AND instructorstatuses.rank != 'TEACHING ASSOCIATE' "
          "AND instructorstatuses.rank != 'INSTRUCTOR/LECT A'")))))


(define working
  (for/list ([l (file->lines "tt-non-tt.csv")])
    (regexp-split #px", *" l)))

(unless (= (length working)
           (set-count (list->set (map first working))))
  (error "name duplicate!"))

(define tt
  (map first
       (filter (lambda (x) (equal? (second x) "tt")) working)))
(define non-tt
  (map first
       (filter (lambda (x) (equal? (second x) "non-tt")) working)))
(define unk
  (map first
       (filter (lambda (x) (equal? (second x) "unknown")) working)))

(unless (= (+ (length tt) (length non-tt) (length unk))
           (length working))
  (error "sum is wrong!"))


(define must-be-non-tt
  (set-subtract 
   (list->set unk)
   (list->set rows)))

(define still-unknown
  (set-intersect
   (list->set unk)
   (list->set rows)))



(length tt)

(define tagged-by-wayne-assoc
  (for/list ([l (file->lines "/tmp/g.txt")])
    (rest (regexp-split #px"\t" l))))
(define tagged-by-wayne (map first tagged-by-wayne-assoc))

(map (lambda (n)
       (assoc n tagged-by-wayne-assoc))
     (set->list (set-intersect still-unknown (list->set tagged-by-wayne))))


(with-output-to-file "/tmp/final-tt-2.csv"
  (lambda ()
    (for ([n tt])
      (display (~a n"\n")))))

#|
(define instructors 
  (for/list ([r rows])
    (rest (regexp-match #px"([A-Z]) ([A-Z ]) ([A-Z]*)" 
                        (vector-ref r 0)))))

(define non-tt-lastnames-assoc
  (remove-duplicates
   (for/list ([r (file->lines "non-tt-list.csv")])
     (define fields (regexp-split #px", *" r))
     (list (list
            (string-upcase (substring (second fields) 0 1))
            (string-upcase (first fields)))
           (take fields 2)))))
(define non-tt-lastnames (map first non-tt-lastnames-assoc))

(define tt-lastnames-assoc
  (remove-duplicates
   (for/list ([r (file->lines "tt-list.csv")])
     (define fields (regexp-split #px", *" r))
     (list (list
            (string-upcase (substring (second fields) 0 1))
            (string-upcase (first fields)))
           (take fields 2)))))
(define tt-lastnames (map first tt-lastnames-assoc))


(define matchings
  (for/list ([n instructors])
    (define id (list (first n) (third n)))
    (define is-tt? (not (not (member id tt-lastnames))))
    (define is-non-tt? (not (not (member id non-tt-lastnames))))
    (match (list is-tt? is-non-tt?)
      [(list #t #t)
       (display (~a "is both: "n"\n"))
       (list n 'unknown)]
      [(list #t #f)
       (list n 'tt)]
      [(list #f #t)
       (list n 'non-tt)]
      [(list #f #f)
       (display (~a "is neither: "n"\n"))
       (display (~a " possible matches in tt: "
                    (map (lambda (m)
                           (second (assoc m tt-lastnames-assoc)))
                         (filter (lambda (x)
                                   (equal? (third n) (second x)))
                                 tt-lastnames))
                    "\n"))
       (display (~a " possible matches in non-tt: "
                    (map (lambda (m)
                           (second (assoc m non-tt-lastnames-assoc)))
                         (filter (lambda (x)
                                   (equal? (third n) (second x)))
                                 non-tt-lastnames))
                    "\n"))
       (list n 'unknown)])))

(with-output-to-file "/tmp/mappings.rktd"
  (lambda ()
    (for ([m matchings])
      (match-define (list fi mi last) (first m))
      (display (~a fi" "mi" "last", "(second m)"\n")))))
|#
(disconnect conn)