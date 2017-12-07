#lang typed/racket

(require "one-quarter-data.rkt"
         "parsed-data-defn.rkt"
         (only-in "pages-to-parsed-tr.rkt"
                  expected-courseline-fields))

(define qtr 2172)
(define 2172-data
  (qtr->parsed 2172))


(define spreadsheet-courseline-fields : (Listof Symbol)
  (cons 'qtr expected-courseline-fields)
  #;(list 'subject 'course-num  'section ;; appear in all levels, used as all or part of primary key
        'discipline 'level 'enrollment 'classification 'a-ccu 'group-code ;; specific to offering record
        'instructor ;; added to earlier 3 to form primary key of offerfac
        'scu 'faculty-contact-hours 'direct-wtu ;; other fields of offerfac
        'sequence ;; added to earlier 4 to form primary key of offerseq
        'days 'time-start 'time-stop 'tba-hours 'facility ;; ...
        'space 'facility-type 'team-teaching-frac ;; other fields of offerseq
        'indirect-wtu 'total-wtu ;; always blank
        ))

;; a map from label to index
(define column-hash : (Immutable-HashTable Symbol Natural)
  (make-immutable-hash
   (for/list : (Listof (Pairof Symbol Natural))
     ([i : Natural (in-naturals)]
      [label : Symbol (in-list spreadsheet-courseline-fields)])
     (cons (ann label Symbol) (ann i Natural)))))

(define offering-table : (Listof (List (Offering -> Any) Symbol))
  (list (list Offering-subject 'subject)
        (list Offering-coursenum 'course-num)
        (list Offering-section 'section)
        (list Offering-discipline 'discipline)
        (list Offering-level 'level)
        (list Offering-enrollment 'enrollment)
        (list Offering-classification 'classification)
        (list Offering-accu 'a-ccu)
        (list Offering-groupcode 'group-code)))

;; given an offering, produce a list representing one row of a spreadsheet
(define (flatten-offering [o : Offering]) : (Listof Any)
  (define idx-val-mapping
    (cons (cons (hash-ref column-hash 'qtr) qtr) 
    (for/list : (Listof (Pairof Natural Any))
      ([entry (in-list offering-table)])
      (cons (hash-ref column-hash (second entry)) ((first entry) o)))))
  (for/list
    ([i : Natural (in-range (length expected-courseline-fields))])
    (match (assoc i idx-val-mapping)
      [(cons _ val) val]
      [#f ""])))

;; convert a list of CSV values into a single string
(define (csv-row->string [l : (Listof Any)]) : String
  (apply string-append
         (add-between (map elt->csvstr l) ",")))

;; convert a value to a string appropriate for inclusion in a CSV file
(define (elt->csvstr [elt : Any]) : String
  (cond [(string? elt)
         (cond [(regexp-match #px"\"" elt)
                (raise-argument-error 'elt->csvstr
                                      "list of elements not containing a string with a double-quote"
                                      0 elt)]
               [else (string-append "\"" elt "\"")])]
        [(exact-integer? elt)
         (number->string elt)]
        [(real? elt)
         ;; not sure about this... don't want a fraction, though.
         (number->string (exact->inexact elt))]
        [(false? elt)
         "FALSE"]
        [else
         (error 'elt->csvstr
                "unexpected value in list: ~e"
                elt)]))


(define header-row
  (map symbol->string spreadsheet-courseline-fields))

(define csv-rows
  (cons header-row
        (map flatten-offering (Parsed-offerings 2172-data))))

(call-with-output-file "/tmp/zz.csv"
  #:exists 'truncate
  (λ ([port : Output-Port])
    (for ([line (in-list csv-rows)])
      (displayln (csv-row->string line) port))))


;; we're combining a bunch of different kinds of records


