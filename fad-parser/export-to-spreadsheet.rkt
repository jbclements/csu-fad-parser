#lang typed/racket

(require "one-quarter-data.rkt"
         "parsed-data-defn.rkt"
         (only-in "pages-to-parsed-tr.rkt"
                  expected-courseline-fields))

(define qtrs
  
  (qtrs-available)
  #;'(2172))



;; we need a special-case flattener for Split Appts... they're lists.
(define (flattened-instructor-split-appt [i : Instructor]) : String
  (apply string-append
         (add-between (map ~a (Instructor-split-appt i))
                      "/")))

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
        (list FacultyOffering-instructor 'instructor-name)
        (list FacultyOffering-scu 'scu)
        (list FacultyOffering-contact-hours 'faculty-contact-hours)
        (list FacultyOffering-dwtu 'direct-wtu)))

(define offerseq-table : (MappingTable OfferingSequence)
  (list
   (list OfferingSequence-subject 'subject)
   (list OfferingSequence-coursenum 'course-num)
   (list OfferingSequence-section 'section)
   (list OfferingSequence-instructor 'instructor-name)
   (list OfferingSequence-sequence 'sequence)
   (list OfferingSequence-days 'days)
   (list OfferingSequence-time-start 'time-start)
   (list OfferingSequence-time-stop 'time-stop)
   (list OfferingSequence-tba-hours 'tba-hours)
   (list OfferingSequence-facility 'facility)
   (list OfferingSequence-space 'space)
   (list OfferingSequence-facility-type 'facility-type)
   (list OfferingSequence-team-teach-frac 'team-teaching-frac)))

(define instructor-table : (MappingTable Instructor)
  (list
   (list Instructor-name 'instructor-name)
   (list Instructor-id 'id)
   (list Instructor-other-id 'other-id)
   (list Instructor-rank 'rank)
   (list Instructor-adm-level 'adm-level)
   (list Instructor-tsf 'tsf)
   (list Instructor-iaf 'iaf)
   (list Instructor-osf 'osf)
   (list flattened-instructor-split-appt 'split-appt)
   (list Instructor-split-frac 'split-frac)
   (list Instructor-iff 'iff)
   ;; shouldn't appear in spreadsheet
   ;(list Instructor-summary 'summary)
   ;; these will have to be separate lines
   ;(list Instructor-specials 'specials)
   (list Instructor-home? 'home?)))

(define special-table : (MappingTable Special)
  (list
   (list Special-description 'description)
   (list Special-scu 'scu)
   (list Special-contact-hours 'faculty-contact-hours)
   (list Special-direct-wtu 'direct-wtu)
   (list Special-indirect-wtu 'indirect-wtu)))



(define expected-instructor-fields (cons 'dept (map (inst second Any Symbol) instructor-table)))

(unless (set-empty? (set-intersect (list->set expected-instructor-fields)
                                   (list->set expected-courseline-fields)))
  (error 'spreadsheet-fields
         "expected no overlap, found overlapping fields: ~e"
         (set-intersect (list->set expected-instructor-fields)
                        (list->set expected-courseline-fields))))


(define spreadsheet-fields : (Listof Symbol)
  (append (list 'qtr 'kind)
          expected-instructor-fields
          (list 'description) ;; used for specials
          (list 'subject 'course-num  'section ;; appear in all levels, used as all or part of primary key
                'discipline 'level 'enrollment 'classification 'a-ccu 'group-code ;; specific to offering record
                ;; using earlier instructor-name instead...
                ;;'instructor ;; added to earlier 3 to form primary key of offerfac
                'scu 'faculty-contact-hours 'direct-wtu ;; other fields of offerfac
                'sequence ;; added to earlier 4 to form primary key of offerseq
                'days 'time-start 'time-stop 'tba-hours 'facility ;; ...
                'space 'facility-type 'team-teaching-frac ;; other fields of offerseq
                'indirect-wtu ;; used for specials
                'total-wtu ;; always blank
                )))

(when (check-duplicates spreadsheet-fields)
  (error 'spreadsheet-fields "duplicate spreadsheet column name: ~e"
         (check-duplicates spreadsheet-fields)))

;; a map from label to index
(define column-hash : (Immutable-HashTable Symbol Natural)
  (make-immutable-hash
   (for/list : (Listof (Pairof Symbol Natural))
     ([i : Natural (in-naturals)]
      [label : Symbol (in-list spreadsheet-fields)])
     (cons (ann label Symbol) (ann i Natural)))))






;; given a table and a value, produce an association list representing a row of a spreadsheet:
(: flatten-struct (All (T) ((MappingTable T) -> (T -> (Listof (Pairof Symbol Any))))))
(define ((flatten-struct table) o)
  (for/list : (Listof (Pairof Symbol Any))
    ([entry (in-list table)])
    (cons (second entry) ((first entry) o))))

;; given a symbol-any assoc list, map it to a spreadsheet row (a list of strings)
;; by looking up each column's location
(: assoc->csv-row ((Listof (Pairof Symbol Any)) -> (Listof Any)))
(define (assoc->csv-row row-assoc)
  ;; look up the index for each column:
  (define idx-val-mapping
    (for/list : (Listof (Pairof Natural Any))
      ([pr (in-list row-assoc)])
      (cons (hash-ref column-hash (car pr)) (cdr pr))))
  ;; put them in order:
  (for/list
    ([i : Natural (in-range (length spreadsheet-fields))])
    (match (assoc i idx-val-mapping)
      [(cons _ val) val]
      [#f ""])))

(: ordinary-flattener (All (T) ((MappingTable T) String -> (Natural -> (T -> (Listof Any))))))
(define (((ordinary-flattener table kind) qtr) offering)
  (assoc->csv-row
   (append
    (list (cons 'qtr qtr)
          (cons 'kind kind))
    ((flatten-struct table) offering))))

(: flatten-instructor (Natural String -> (Instructor -> (Listof Any))))
(define ((flatten-instructor qtr dept) instructor)
  (assoc->csv-row
   (append
    (list (cons 'qtr qtr)
          (cons 'kind "instructor")
          (cons 'dept dept))
    ((flatten-struct instructor-table) instructor))))

(: flatten-special (Natural String String -> (Special -> (Listof Any))))
(define ((flatten-special qtr dept instructor) special)
  (assoc->csv-row
   (append
    (list (cons 'qtr qtr)
          (cons 'kind "non-course")
          (cons 'dept dept)
          (cons 'instructor-name instructor))
    ((flatten-struct special-table) special))))



;; given an offering, produce a list representing one row of a spreadsheet

(define flatten-offering (ordinary-flattener offering-table "offering"))
(define flatten-offerfac (ordinary-flattener offerfac-table "faculty-offering"))
(define flatten-sequence (ordinary-flattener offerseq-table "offering-sequence"))

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
        [(equal? #t elt)
         "TRUE"]
        [else
         (error 'elt->csvstr
                "unexpected value in list: ~e"
                elt)]))


(define header-row
  (map symbol->string spreadsheet-fields))

;; given a quarter, parse and return the spreadsheet rows
(define (qtr->csv-rows [qtr : Natural])
  (define data (qtr->parsed qtr))

  (append
   (map (flatten-offering qtr) (Parsed-offerings data))
   (map (flatten-offerfac qtr) (Parsed-faculty-offerings data))
   (map (flatten-sequence qtr) (Parsed-sequences data))
   (apply
    append
    (for/list : (Listof (Listof (Listof Any)))
      ([dept : Dept (ann (in-list (Parsed-depts data))
                         (Sequenceof Dept))])
     (map (flatten-instructor qtr (Dept-name dept)) (Dept-instructors dept))))
   (apply
    append
    (for/list : (Listof (Listof (Listof Any)))
      ([dept : Dept (ann (in-list (Parsed-depts data))
                         (Sequenceof Dept))])
      (apply
       append
       (for/list : (Listof (Listof (Listof Any)))
         ([instructor : Instructor (ann (in-list (Dept-instructors dept))
                                        (Sequenceof Instructor))])
         (map (flatten-special qtr (Dept-name dept) (Instructor-name instructor))
              (Instructor-specials instructor))))))))

(define rows (apply append (map qtr->csv-rows qtrs)))

(call-with-output-file "/tmp/zz.csv"
  #:exists 'truncate
  (λ ([port : Output-Port])
    (for ([line (in-list (cons header-row rows))])
      (displayln (csv-row->string line) port))))


;; we're combining a bunch of different kinds of records


