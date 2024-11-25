#lang racket

;; problems with TR/Racket interaction, commenting out for now:

#;(

(require "pages-to-parsed-tr.rkt"
         "one-quarter-data.rkt"
         "parsed-data-defn.rkt"
         csse-scheduling/qtr-math)

(define (as-int s)
  (real-as-int (string->number/0 s)))

(define (real-as-int r)
  (inexact->exact (round (* 1000 r))))

(define (normalize-rank rank)
  (match rank
    ["ASSISTANT PRF/LECT B"  "ASSISTANT PRF/LECT B"]
    ["TEACHING ASST/LECT L"  "TEACHING ASST/LECT L"]
    ["INSTRUCTOR/LECT A"  "INSTRUCTOR/LECT A"]
    ["PROFESSOR/LECT D"  "PROFESSOR/LECT D"]
    ["ADMINISTRATOR"  "ADMINISTRATOR"]
    ["TEACHING ASSOCIATE"  "TEACHING ASSOCIATE"]
    ["TCHNG ASSOCIATE"  "TEACHING ASSOCIATE"]
    ["ASSOC PROF/LECT C"  "ASSOCIATE PRF/LECT C"]
    ["ASSOCIATE PRF/LECT C"  "ASSOCIATE PRF/LECT C"]
    ["OTHER"  "OTHER"]
    ["ASST PROF/LECT B"  "ASSISTANT PRF/LECT B"]
    ))

;; export a set of records to a file.
(define (export-data filename records)
  (with-output-to-file filename
    (lambda ()
      (for ([c (in-list records)])
        (define data
          (for/list ([d (in-list c)])
            (match d
              [#f "\\N"]
              ['null "\\N"]
              [(? string? s)
               (when (ormap (λ(ch)
                              (member ch '(#\tab #\newline #\\)))
                            (string->list s))
                 (error 'export-data
                        "no bad chars allowed in strings: ~e\n"
                        s))
               s]
              [(? exact-integer? i) i])))
        (display (apply ~a (append (add-between data "\t") (list "\n"))))))
    #:exists 'truncate))


(define (all-instructors q qtr)
  (remove-duplicates
   (apply
    append
    (for/list  ([dept (Parsed-depts q)])
      (for/list ([instr (Dept-instructors dept)])
        (define hdr (Instructor-header instr))
        (define soc (match (col-ref 'id hdr)
                      [(regexp #px"0 (.*)" (list dc soc)) soc]
                      [(regexp #px"XXXXX[0-9]{4}" (list soc)) soc]))
        (list (col-ref 'name hdr)
              (col-ref 'name hdr)
              soc))))))

(define (all-instructor-statuses parsed-qtr qtr)
  (apply
   append
   (for/list  ([dept (Parsed-depts parsed-qtr)])
     (for/list ([instr (Dept-instructors dept)]
                #:when (Instructor-home? instr))
       (define hdr (Instructor-header instr))
       (list
        (col-ref 'name hdr)
        qtr
        (Dept-name dept)
        (as-int (col-ref 'tsf hdr))
        (as-int (col-ref 'iaf hdr))
        (as-int (col-ref 'osf hdr))
        (col-ref 'adm-lvl hdr)
        (normalize-rank (col-ref 'rank hdr)))))))



#;(define all-specials
  (apply
   append
  (for/list ([q parsed-qtrs]
             [qtr qtr-nums])
    (for*/list  ([dept (Parsed-depts q)]
                 [instr (Dept-instructors dept)]
                 [course (Instructor-courses instr)]
                 #:when (special? course))
      (when (not (string=? (col-ref/g 'scu course) ""))
        (error))
      (when (not (string=? (col-ref/g 'faculty-contact-hours course) ""))
        (raise-argument-error
         'all-specials
         "special course without faculty contact hours"
         0 course))
      (list ""
            (instructor-name instr)
            qtr
            (inexact->exact
             (* 1000 (string->number/0 (col-ref/g 'direct-wtu course))))
            (inexact->exact
             (* 1000 (string->number/0 (col-ref/g 'indirect-wtu course))))
            (col-ref/g 'special course))))))

#;(define (export-specials)
  (export-data "/tmp/specialcredits.txt" all-specials))

;; no datapoints; can't confirm that specials can occur for a person in 
;; more than one department report.
#;(for ([q parsed-qtrs]
      [qtr qtr-nums])
  (define table
    (group-by 
     (remove-duplicates
      (for*/list  ([dept (parsed-depts q)]
                   [instr (dept-instructors dept)]
                   [course (instructor-courses instr)]
                   #:when (special? course))
        (list (instructor-name instr)
              (dept-name dept))))
     first))
  (for ([(k v) (in-hash table)])
    (when (< 1 (length v))
      (display (~a "got one: " v)))))


;; ensure all elements are the same, return the first
;; (forall A . (nonempty-listof A) -> A)
(define (ensure-same l)
  (unless (cons? l)
    (raise-argument-error 'ensure-same "nonempty list" 0 l))
  (unless (andmap (lambda (elt) (equal? elt (first l))) l)
    (raise-argument-error 'ensure-same "list of identical elements" 0 l))
  (first l))

(define (remove-empties l)
  (filter (lambda (elt) (not (string=? elt ""))) l))


(define (all-offerings parsed-qtr qtr)
  (for/list ([o (in-list (Parsed-offerings parsed-qtr))])
    (list qtr
          (Offering-subject o)
          (Offering-coursenum o)
          (Offering-section o)
          (Offering-discipline o)
          (Offering-level o)
          (Offering-enrollment o)
          (Offering-classification o)
          (real-as-int (Offering-accu o))
          (Offering-groupcode o))))

(define (all-offerfacs parsed-qtr qtr)
  (for/list ([o (in-list (Parsed-faculty-offerings parsed-qtr))])
    (list qtr
          (FacultyOffering-subject o)
          (FacultyOffering-coursenum o)
          (FacultyOffering-section o)
          (FacultyOffering-instructor o)
          (real-as-int (FacultyOffering-scu o))
          (real-as-int (FacultyOffering-contact-hours o))
          (real-as-int (FacultyOffering-dwtu o)))))

(define (export-instructor-statuses parsed-qtr qtr)
  (export-data
   (~a "/tmp/instructorstatuses-"qtr".txt")
   (all-instructor-statuses parsed-qtr qtr)))

(define (export-offerings parsed-qtr qtr)
  (export-data
   (~a "/tmp/offerings-"qtr".txt")
   (all-offerings parsed-qtr qtr)))

(define (export-offerfacs parsed-qtr qtr)
  (export-data
   (~a "/tmp/offerfacs-"qtr".txt")
   (all-offerfacs parsed-qtr qtr)))

(define (export-instructors parsed-qtr qtr)
  (export-data
   (~a "/tmp/instructors-"qtr".txt")
   (all-instructors parsed-qtr qtr)))

(define earliest-qtr 2088)
(define after-last-qtr 2178)
;; compute the incremental additions for this quarter:
(define only-adding 2174)

(for ([qtr (in-list (qtrs-in-range earliest-qtr after-last-qtr))])
  (define parsed-qtr (qtr->parsed qtr))
  (export-instructor-statuses parsed-qtr qtr)
  (export-offerings parsed-qtr qtr)
  (export-offerfacs parsed-qtr qtr)
  (export-instructors parsed-qtr qtr))

;; compute new instructors:
(define existing-instructors
  (apply
   append
   (for/list ([qtr (in-list (qtrs-in-range earliest-qtr only-adding))])
     (file->lines (~a "/tmp/instructors-"qtr".txt")))))
(define new-instructors
  (remove* existing-instructors
           (file->lines (~a "/tmp/instructors-"only-adding".txt"))))
(call-with-output-file (~a "/tmp/new-instructors-"only-adding".txt")
    (λ (port)
      (for ([l (in-list new-instructors)])
        (fprintf port "~a\n" l))))




#;(define (corrected course)
  (cond 
    [(and (has-group? course)
          (is-senior-project? course))
     (error 'corrected-dwtus
            "can't deal with senior project courses"
            " that also have group codes.")]
    [(is-senior-project? course)
     (list 36
           (* (string->number
               (col-ref/g 'enrollment course))
              (string->number
               (col-ref/g 'team-teaching-frac course))
              1/3))]
    [else
     (with-handlers ([exn:fail?
                      (λ (exn)
                        (fprintf (current-error-port)
                                 "error on course: ~v\n"
                                 course)
                        (raise exn))])
         (list (string->number/0 (course-classification course))
           (nums-cleanup (col-ref/g 'direct-wtu course))))]))

(define (nums-cleanup l)
  (match l
    ["" 0.0]
    [(? string? n) (match (string->number n)
                     [#f (error 'nums-cleanup
                                "expected a number string: ~e"
                                l)]
                     [(? number? n) n])]
    [(list (? string? s)) (string->number s)]))


#;(define all-offerseqs
  (apply
   append
   (for/list ([q parsed-qtrs]
             [qtr qtr-nums])
     (printf "qtr: ~v\n" qtr)
    (apply
     append
    (for*/list  ([dept (parsed-depts q)]
                 [instr (dept-instructors dept)])
      (for/list ([course (instructor-courses instr)]
                 #:when (not (special? course)))
        (match-define (list newclass newdwtus)
          (corrected course))
        (define seqnums (col-ref/g 'sequence course))
        (for/list ([seq (in-list seqnums)]
                   [ttf (in-list col-ref/g 'team-teaching-frac course)]
                   [scu (in-list col-ref/g 'scu course)]
                   [fch (in-list col-ref/g 'fch course)]
                   [dwtu])
          )
        
        `(,qtr
          ,(col-ref/g 'dept course)
          ,(col-ref/g 'course-num course)
          ,(col-ref/g 'section course)
          ,seq
          ,(instructor-name instr))
        
        (define good-idx
          (for/first ([seqnum (in-list seqnums)]
                      [i (in-naturals)]
                      #:when (regexp-match 
                              #px"^\\*" seqnum))
            i))
        (append
         
         (map 
          (lambda (col)
            (inexact->exact
             (round (* 1000 (string->number/0 (col-ref/g col course))))))
          '(team-teaching-frac
            scu
            faculty-contact-hours
            direct-wtu))
         (list newclass
               (inexact->exact
                (round (* 1000 newdwtus)))
               (list-ref (col-ref/g 'facility-type course)
                         good-idx)))))))))

#;(define (export-offerseqs)
  (export-data "/tmp/offerseqs.txt" all-offerseqs))


#;(export-groupings-relation)
#;(export-offerseqs))

