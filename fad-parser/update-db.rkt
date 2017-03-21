#lang racket

;; instructor data needs to be merged, not just added.
;; this file does that.

(require "pages-to-parsed-tr.rkt"
         "one-quarter-data.rkt"
         "make-connection.rkt"
         db)


;; given a filename and a list of records (lists),
;; output the records in tab-separated format to the given filename
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

(define conn (make-connection))

(require explorer)

(define existing-instructors
  (map vector->list
       (query-rows conn
                   "SELECT * FROM instructors;")))

(define qtr-nums '(2172))
(define parsed-qtrs (map qtr->parsed qtr-nums))

(define (as-int s)
  (real-as-int (string->number/0 s)))

(define (real-as-int r)
  (inexact->exact (round (* 1000 r))))

(define all-instructors
  (remove-duplicates
   (apply
    append
    (for/list ([q parsed-qtrs]
               [qtr qtr-nums])
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
                 soc))))))))

(define new-instructors
  (remove* existing-instructors all-instructors))



new-instructors

;; oops... not enough privileges.
(define (add-new-instructors!)
  (for/list ([i (in-list new-instructors)])
    (apply
     query-exec conn
     "INSERT INTO instructors VALUES ($1,$2,$3);"
     i)))

(define (export-instructors)
  (export-data "/tmp/instructors.txt" new-instructors))

(error 'stop "here")

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

(define all-instructor-statuses
  (apply
   append
   (for/list ([q parsed-qtrs]
              [qtr qtr-nums])
     (apply
      append
      (for/list  ([dept (Parsed-depts q)])
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
           (normalize-rank (col-ref 'rank hdr)))))))))

(define (export-instructor-statuses)
  (export-data  "/tmp/instructorstatuses.txt" all-instructor-statuses))



#;(define (export-course-names)
  (export-data "/tmp/courses.txt" all-course-names))

;; no point in making this a table.
#;(define all-subjects
  (remove-duplicates (map list (map first all-course-names))))

#;(define (export-subjects)
  (export-data "/tmp/subjects.txt" all-subjects))

#;((define all-specials
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

(define (export-specials)
  (export-data "/tmp/specialcredits.txt" all-specials)))

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


(define all-offerings
  (apply
   append
   (for/list ([q parsed-qtrs]
              [qtr qtr-nums])
     (for/list ([o (in-list (Parsed-offerings q))])
       (list qtr
             (Offering-subject o)
             (Offering-coursenum o)
             (Offering-section o)
             (Offering-discipline o)
             (Offering-level o)
             (Offering-enrollment o)
             (Offering-classification o)
             (real-as-int (Offering-accu o))
             (Offering-groupcode o))))))

(define (export-offerings)
  (export-data "/tmp/offerings.txt" all-offerings))

(define all-offerfacs
  (apply
   append
   (for/list ([q parsed-qtrs]
              [qtr qtr-nums])
     (for/list ([o (in-list (Parsed-faculty-offerings q))])
       (list qtr
             (FacultyOffering-subject o)
             (FacultyOffering-coursenum o)
             (FacultyOffering-section o)
             (FacultyOffering-instructor o)
             (real-as-int (FacultyOffering-scu o))
             (real-as-int (FacultyOffering-contact-hours o))
             (real-as-int (FacultyOffering-dwtu o)))))))

(define (export-offerfacs)
  (export-data "/tmp/offerfacs.txt" all-offerfacs))


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
#;(export-offerseqs)

