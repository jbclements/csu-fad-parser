#lang racket

;; 2025-03-28: Hack is partly fixed. updated numeric instructions below:
;; SEE 2234-hack-fix.rkt, probably needs to be applied to all new FADs going forward, sigh...

;; WHOA! the FAD has had EMPLIDs since 2168. Instructor information
;; needs to be overhauled. In particular, there are definitely
;; some ids that should be combined. I don't think there are any
;; that need to be split, but this is a concern.

;; To fix this, probably do something like this:
;; - update FAD parser to emit emplids for 2168+ if it doesn't already.
;; - use brain to discover whether there are ids that can be combined,
;;    or ids that should be separated
;; - use this information for a painful database update
;; - going forward, add a potential remapping step to the fad parser
;;    to map name/emplid to id.

;; I do not have time to do this right now, ugh.

;; this file updates the database with information from
;; new FAD reports. Generally speaking, after receiving
;; a new FAD, you have to run functions from this file
;; to generate .tsv files, then upload them and use
;; \COPY to add them.

;; 1) update the qtr-nums, below
;; 1.5) Delete .tsv files from /tmp
;; 1.75) use "2234-hack-fix.rkt" to repair fad file, following instructions in that
;;       file
;; 2) run
;; 2.5) Use fad-name-add! from instructor-name-ids.rkt, but only for
;;      instructors in our departments
;; 3) by hand, run all the functions labeled RUN ME, below.
;;   here's what's there now:
;; - (export-instructors)
;; - (export-instructor-statuses)
;; - (export-offerings)
;; - (export-offerfacs)
;; 4) schlep the resulting .tsv files over to johnlewis
;; 5) back up using pg_dump fad > /tmp/fad.sql && mv /tmp/fad.sql ~
;; 5) psql fad
;; 6) copy tsvs into database:
#|
psql -f $HOME/fad-update-commands.txt fad

OR

\COPY instructors FROM '/tmp/instructors.tsv';
\COPY instructorstatuses FROM '/tmp/instructorstatuses.tsv';
\COPY offerings FROM '/tmp/offerings.tsv';
\COPY offerfacs FROM '/tmp/offerfacs.tsv';

|#
 
#;(for ([i (in-list '("instructors"
                    "instructorstatuses"
                    "offerings"
                    "offerfacs"))])
  (displayln (~a "\\COPY "i" FROM '/tmp/"i".tsv';")))

(require "one-quarter-data.rkt"
         "make-connection.rkt"
         "tsv-export.rkt"
         "parsed-data-defn.rkt"
         json
         db)


;; these are the quarters you're running on. Typically
;; just a single quarter.
(define qtr-nums #;(qtrs-available) '(2252))

;; given a filename and a list of records (lists),
;; output the records in tab-separated format to the given filename
(define (export-data filename records)
  (tsv-export filename records))

(printf "if you don't have a tunnel open, this may fail...\n")
(flush-output)
(define conn (make-connection))

(define existing-instructors
  (map vector->list
       (query-rows conn
                   "SELECT * FROM instructors;")))


(define parsed-qtrs (map qtr->parsed qtr-nums))


(define (as-int s)
  (real-as-int s))

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
           ;(define hdr (Instructor-header instr))
           (define soc (match (Instructor-id instr)
                         [(regexp #px"0 (.*)" (list dc soc)) soc]
                         [(regexp #px"XXXXX[0-9]{4}" (list soc)) soc]))
           (list (Instructor-name instr)
                 (Instructor-name instr)
                 soc
                 ;; HACK HACK
                 sql-null))))))))


(define new-instructors
  (remove* existing-instructors all-instructors))


"new instructors:"
new-instructors

;; unused
#;(define (add-new-instructors!)
  (for/list ([i (in-list new-instructors)])
    (apply
     query-exec conn
     "INSERT INTO instructors VALUES ($1,$2,$3);"
     i)))

;; RUN ME
(define (export-instructors)
  (export-data "/tmp/instructors.tsv" new-instructors))





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
          (list
           (Instructor-name instr)
           qtr
           (Dept-name dept)
           (as-int (Instructor-tsf instr))
           (as-int (Instructor-iaf instr))
           (as-int (Instructor-osf instr))
           (Instructor-adm-level instr)
           (normalize-rank (Instructor-rank instr)))))))))

;; RUN ME
(define (export-instructor-statuses)
  (export-data  "/tmp/instructorstatuses.tsv" all-instructor-statuses))


;; We're no longer tracking this; any course name is allowed.
#;(define (export-course-names)
  (export-data "/tmp/courses.tsv" all-course-names))

;; ditto
#;(define all-subjects
  (remove-duplicates (map list (map first all-course-names))))

;; no point in making this a table.
#;(define (export-subjects)
  (export-data "/tmp/subjects.tsv" all-subjects))


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
  (export-data "/tmp/specialcredits.tsv" all-specials)))


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

;; no datapoints; can't confirm that specials can occur for a person in 
;; more than one department report.
(define (ensure-same l)
  (unless (cons? l)
    (raise-argument-error 'ensure-same "nonempty list" 0 l))
  (unless (andmap (lambda (elt) (equal? elt (first l))) l)
    (raise-argument-error 'ensure-same "list of identical elements" 0 l))
  (first l))


;; ensure all elements are the same, return the first
;; (forall A . (nonempty-listof A) -> A)
(define (remove-empties l)
  (filter (lambda (elt) (not (string=? elt ""))) l))

(define (val-or-null n)
  (cond [(not n) 'null]
        [else n]))

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
             (val-or-null (Offering-classification o))
             (real-as-int (Offering-accu o))
             (val-or-null (Offering-groupcode o)))))))

;; RUN ME
(define (export-offerings)
  (export-data "/tmp/offerings.tsv" all-offerings))

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

;; RUN ME
(define (export-offerfacs)
  (export-data "/tmp/offerfacs.tsv" all-offerfacs))


(define (export-summaries)
  ;; given a list of (List String (Listof String)), return a hash
  (define (lolos->hash lols)
    (for/list ([line (in-list lols)])
      (match-define (list title nums) line)
      (hash 'line-title title
            'num-strs nums)))
  (define all-hashes
    (for/list ([qtr-num (in-list qtr-nums)]
               [parsed (in-list parsed-qtrs)])
      (match parsed
        [(Parsed college-summary depts _ _ _)
         (make-immutable-hash
          (list (cons 'qtr qtr-num)
                (cons 'college-summary
                      (match college-summary
                        ['no-college-summary-page
                         "no college summary page this qtr"]
                        [else (lolos->hash (Parsed-college-summary parsed))]))
                (cons 'dept-summaries
                      (map (λ (dept)
                             (hash 'dept (Dept-name dept)
                                   'dept-summary
                                   (lolos->hash (Dept-summary dept))))
                           (Parsed-depts parsed)))))])))
  (call-with-output-file "/tmp/summaries.json"
    (λ (port)
      (write-json all-hashes port))))


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
    (export-data "/tmp/offerseqs.tsv" all-offerseqs))


#;(export-groupings-relation)
#;(export-offerseqs)

