#lang typed/racket

(require "one-quarter-data.rkt"
         "parsed-data-defn.rkt"
         (only-in "pages-to-parsed-tr.rkt"
                  expected-courseline-fields))

(define qtrs
  (qtrs-available)
  #;'(2172))


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

;; a mapping table is used to indicate which fields go in which columns
;; of the spreadsheet.
(define-type MappingTable (All (T) (Listof (List (T -> Any) Symbol))))

(define offering-table : (MappingTable Offering)
  (list (list Offering-subject 'subject)
        (list Offering-coursenum 'course-num)
        (list Offering-section 'section)
        (list Offering-discipline 'discipline)
        (list Offering-level 'level)
        (list Offering-enrollment 'enrollment)
        (list Offering-classification 'classification)
        (list Offering-accu 'a-ccu)
        (list Offering-groupcode 'group-code)))

(define offerfac-table : (MappingTable FacultyOffering)
  (list (list FacultyOffering-subject 'subject)
        (list FacultyOffering-coursenum 'course-num)
        (list FacultyOffering-section 'section)
        (list FacultyOffering-instructor 'instructor)
        (list FacultyOffering-scu 'scu)
        (list FacultyOffering-contact-hours 'faculty-contact-hours)
        (list FacultyOffering-dwtu 'direct-wtu)))

(define offerseq-table : (MappingTable OfferingSequence)
  (list
   (list OfferingSequence-subject 'subject)
   (list OfferingSequence-coursenum 'course-num)
   (list OfferingSequence-section 'section)
   (list OfferingSequence-instructor 'instructor)
   (list OfferingSequence-sequence 'sequence)
   (list OfferingSequence-days 'days)
   (list OfferingSequence-time-start 'time-start)
   (list OfferingSequence-time-stop 'time-stop)
   (list OfferingSequence-tba-hours 'tba-hours)
   (list OfferingSequence-facility 'facility)
   (list OfferingSequence-space 'space)
   (list OfferingSequence-facility-type 'facility-type)
   (list OfferingSequence-team-teach-frac 'team-teaching-frac)))


;; given a table and a value, produce a list representing one row of a spreadsheet
(: flatten-struct (All (T) ((MappingTable T) -> (Natural -> (T -> (Listof Any))))))
(define (((flatten-struct table) qtr) o)
  (define idx-val-mapping
    (cons (cons (hash-ref column-hash 'qtr) qtr)
    (for/list : (Listof (Pairof Natural Any))
      ([entry (in-list table)])
      (cons (hash-ref column-hash (second entry)) ((first entry) o)))))
  (for/list
    ([i : Natural (in-range (length expected-courseline-fields))])
    (match (assoc i idx-val-mapping)
      [(cons _ val) val]
      [#f ""])))

;; given an offering, produce a list representing one row of a spreadsheet
(define flatten-offering (flatten-struct offering-table))
(define flatten-offerfac (flatten-struct offerfac-table))
(define flatten-sequence (flatten-struct offerseq-table))

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

;; given a quarter, parse and return the spreadsheet rows
(define (qtr->csv-rows [qtr : Natural])
  (define data (qtr->parsed qtr))

  (append
   (map (flatten-offering qtr) (Parsed-offerings data))
   (map (flatten-offerfac qtr) (Parsed-faculty-offerings data))
   (map (flatten-sequence qtr) (Parsed-sequences data))))

(define rows (apply append (map qtr->csv-rows qtrs)))

(call-with-output-file "/tmp/zz.csv"
  #:exists 'truncate
  (λ ([port : Output-Port])
    (for ([line (in-list (cons header-row rows))])
      (displayln (csv-row->string line) port))))


;; we're combining a bunch of different kinds of records


