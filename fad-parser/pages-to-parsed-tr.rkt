#lang typed/racket

;; this is the second stage in parsing; it takes fad-pages,
;; and produces "parsed".

;; it's now written in Typed Racket, increasing my confidence
;; that this code works now and will be more readable and
;; dependable in the future.

(require "divide-columns.rkt"
         "check-primary-keys.rkt"
         "parsed-data-defn.rkt")

(require/typed "fad-to-pages.rkt"
               [#:struct dept-pages
                ([name : String]
                 [summary : (Listof (List String
                                          (Listof String)))]
                 [detail : (Listof Any)])]
               [#:struct fad-pages ([college-summary
                                     : (U (Listof (List String
                                                        (Listof String)))
                                          'no-college-summary-page)]
                                    [depts : (Listof dept-pages)])]
               [file->fad-pages (Path-String Format -> fad-pages)])

(require/typed "parse-dept-lines.rkt"
               [pre-2144-dept-lines-parser
                (-> (Listof Any) String
                    (Listof (List (Listof String) String)))]
               [post-2142-dept-lines-parser
                (-> (Listof Any) String
                    (Listof
                     (U (List 'split-appt-remote String (Listof String) String)
                        (List 'home-dept String (Listof String) (Listof String) String)
                        (List 'no-class-instructor String (Listof String) String))))])


(provide (struct-out InstructorLines)
         file->parsed
         parse-pages
         dept->instructors
         lines->instructor
         instructor-courses
         dept-short-name
         col-ref
         line-kind
         cols-ref
         string->number/0
         small?
         expected-courseline-fields)





;; represents an instructor's lines. The header represents
;; the first line of the instructor's record, the summary
;; represents the last line, the courses represent both the
;; courses and the non-course specials, and home? indicates
;; whether this record is the one in the instructor's home
;; department (for split appointments).
;; Note that because of the way the fad is parsed, this data
;; is *heavily* order-dependent; a second sequence may be
;; missing all information about subject, coursenum, etc.
(define-struct InstructorLines ((header : AssocLine) 
                                (summary : AssocLine)
                                (lines : (Listof KindAssocLine))
                                (home? : Boolean))
  #:transparent)


;; a standard association list, with a 'kind' at the beginning
;; indicating whether it's a course line or a special line.
(define-type KindAssocLine
  (Pairof (List 'kind Symbol)
          AssocLine))

;; NOTES ON CRAZY DATA
;; ahhhhh.... I'm starting to believe that the big change between 2142 and 2144
;; is that they stopped printing the lines in which that instructor had no
;; part.
;;
;; COLUMNS:
;;
;; - sequence: should be unique per subject/coursenum/instructor/line. In early
;;   fads, some sequence numbers start with a star. I believe this indicates that
;;   this sequence has values in the SCU, FCH, and some other fields. I don't
;;   believe that any additional information is conveyed by this star.
;;
;; - classification: in pre-2144, always just one classification, appearing
;; in the first line of an instructor's section record. in Post 2142, blanks
;; are replaced by 00s, and some instructor-section records have all lines at
;; 00. :(
;; - discipline : universally blank-after-first.
;; - level : universally blank-after-first.
;; - enrollment : universally blank-after-first.
;; - a-ccu : until 2138, universally blank-after-first. Then things start to
;;   get weird. David Janzen has a lab class split .4/.6 in 2138. In 2142 again
;;   weird, David has two splits, a 1.3/1.7 and another .4/.5. These are all
;;   on CSC 436. AFTER 2142, it looks like the a-ccu is always nonzero for exactly
;;   one line per *section*, not per instructor-section. In summary: it appears
;;   that this number should be summed and attached to the offering
;;   UPDATE: no, it looks like this is wrong. For pre-2142, it appears that each
;;   instructor-section has the correct number, and that they should not be added.
;;   UPDATE 2: ooh, weird stuff when the classification is 00, as it is for section
;;    71 of LAES 302 in 2154...
;;   OKAY: looks like it should be summed-by-instructor-section in pre-2142, and
;;   summed-by-section in post-2138
;; - days : big tangle of heterogeneous and blank-after-first.
;; - time-start : big tangle of junk
;; - time-stop : big tangle of junk
;; - tba-hours : big tangle of junk
;; - facility : big tangle of junk
;; - facility-type : big tangle of junk
;; - group code : at most one per section
;; ...
;; - scu : ah! good data. exactly one nonzero-nonblank value per instructor-section.
;;   this column suggests that we should actually have an instructor-section table.
;; - contact-hours : same thing! except that many of them are completely blank.
;;   looks like these should be summed per instructor-section
;; - direct-wtu : same thing! should be summed per instructor-section
;; - indirect-wtu : totally blank.
;; - total-wtu : totally blank.

;; - team-teaching-frac : did I miss this, before? number or blank. Looks like they
;;   should sum to one for the ... section? instructor-section? probably instructor-section.
;; FIXME CHECK TTF SUMS TO ONE
;; FIMXE CHECK UNIQUENESS OF SEQUENCE NUMBER ... more generally, lots of primary-key uniqueness checks

;; in offering record:
;; - qtr, subject, coursenum, section, discipline, level, enrollment, a-ccu, group code, classification
;; in offerfac record:
;; - ((qtr,subject,coursenum,section),instructor),scu,contact-hours,direct-wtu
;; in offerseq record:
;; - (qtr,subject,coursenum,section),sequence,instructor),days,time-start,time-stop,tba-hours,facility, space, facility-type, team-teaching-frac





(: file->parsed (Path-String Format -> Parsed))
(define (file->parsed file fformat)
  (parse-pages (file->fad-pages file fformat) fformat))

;; transform a fad-pages into a fad-parsed
(: parse-pages (fad-pages Format -> Parsed))
(define (parse-pages fad-pages fformat)
  (define instructorlineseses
    (for/list : (Listof (Listof InstructorLines))
      ([dept-pages (in-list (fad-pages-depts fad-pages))])
      (dept->instructors dept-pages fformat)))
  (define depts
    (for/list : (Listof Dept)
      ([dept-pages (in-list (fad-pages-depts fad-pages))]
       [instructorlineses (in-list instructorlineseses)])
      (define instructors (map lines->instructor instructorlineses))
      (Dept (dept-short-name (dept-pages-name dept-pages))
            (dept-pages-summary dept-pages)
            instructors)))
  (define instructorlineses (apply append instructorlineseses))
  (define courselines
    (apply append
           (for/list : (Listof (Listof AssocLine))
             ([instructorlines (in-list instructorlineses)])
             (attach-course-identity-info
              (instructor-courses instructorlines)))))
  (check-all-the-same-fields courselines)
  (define section-line-groups (regroup-lines courselines))
  (define offerings (map (section-lines->offering fformat) section-line-groups))
  (check-offering-uniqueness offerings)
  (define offerfacs
    (apply append
           (map section-lines->faculty-offerings section-line-groups)))
  (check-offerfac-uniqueness offerfacs)
  (define sequences (map line->sequence courselines))
  (check-offerseq-uniqueness sequences)
  (Parsed (fad-pages-college-summary fad-pages)
          depts
          offerings
          offerfacs
          sequences))

;; these are the observed (common) fields as of 2017-12-06
;; we're also using them to order the fields of the spreadsheet.
(define expected-courseline-fields
  (list 'subject 'course-num 'section ;; appear in all levels, used as all or part of primary key
        'discipline 'level 'enrollment 'classification 'a-ccu 'group-code ;; specific to offering record
        'instructor ;; added to earlier 3 to form primary key of offerfac
        'scu 'faculty-contact-hours 'direct-wtu ;; other fields of offerfac
        'sequence ;; added to earlier 4 to form primary key of offerseq
        'days 'time-start 'time-stop 'tba-hours 'facility ;; ...
        'space 'facility-type 'team-teaching-frac ;; other fields of offerseq
        'indirect-wtu 'total-wtu ;; always blank
        ))

;; do all of the lines have the same set of fields?
(define (check-all-the-same-fields [courselines : (Listof AssocLine)]) : Void
  (define fields-set (list->set expected-courseline-fields))
  (unless (andmap (λ ([line : AssocLine]) (equal? (line-labels line) fields-set))
                  courselines)
    (error 'check-all-the-same-fields
           "not all lines have the expected set of fields")))

;; return the set of labels in an AssocLine
(define (line-labels [line : AssocLine]) : (Setof Symbol)
  (list->set (map (inst first Symbol Any) line)))


;; given a list of lines representing a section, return an offering
(: section-lines->offering (Format -> (Listof AssocLine) -> Offering))
(define ((section-lines->offering fformat) lines)
  (when (null? lines)
    (raise-argument-error 'section-lines->offering
                          "non-empty list" 0 lines))
  (with-handlers ([exn:fail?
                   (λ ([exn : exn])
                     (fprintf (current-error-port)
                              "error in section-lines: ~v" lines)
                     (raise exn))])
    (Offering (allthesame lines 'subject)
              (allthesame lines 'course-num)
              (assert (string->number (allthesame lines 'section)) nat?)
              (assert (string->number (assert (atmostone lines 'discipline) string?)) nat?)
              (normalize-level (assert (atmostone lines 'level) string?))
              (assert (string->number (assert (atmostone lines 'enrollment) string?)) nat?)
              (collapse-classification lines)
              (match fformat
                ['pre-2144 (sum-by-instructor/allthesame lines 'a-ccu)]
                [_ (sum-of-nums lines 'a-ccu)])
              (match (atmostone lines 'group-code)
                [#f #f]
                [(? string? s) (assert (string->number s) nat?)]))))

(: section-lines->faculty-offerings ((Listof AssocLine) -> (Listof FacultyOffering)))
(define (section-lines->faculty-offerings lines)
  (when (null? lines)
    (raise-argument-error 'section-lines->offering
                          "non-empty list" 0 lines))
  (define instr-groups
    (group-by (λ ([l : AssocLine]) (col-ref 'instructor l))
              lines))
  (map instructor-section-lines->faculty-offering instr-groups))


(: instructor-section-lines->faculty-offering ((Listof AssocLine) -> FacultyOffering))
(define (instructor-section-lines->faculty-offering lines)
  (define subject (allthesame lines 'subject))
  (define course-num (allthesame lines 'course-num))
  (FacultyOffering subject
                   course-num
                   (assert (string->number (allthesame lines 'section)) nat?)
                   (allthesame lines 'instructor)
                   (sum-of-nums lines 'scu)                   
                   (sum-of-nums lines 'faculty-contact-hours)
                   (sum-of-nums lines 'direct-wtu)))

(: line->sequence (AssocLine -> OfferingSequence))
(define (line->sequence line)
  (unless (equal? (col-ref 'indirect-wtu line) "")
    (raise-argument-error 'line->sequence
                          "empty string for 'indirect-wtu"
                          line))
  (unless (equal? (col-ref 'total-wtu line) "")
    (raise-argument-error 'line->sequence
                          "empty string for 'total-wtu"
                          line))
  (define section
    (match (string->number (col-ref 'section line))
      [(? nat? n) n]
      [other (raise-argument-error 'line->sequence
                                   "section field containing number"
                                   line)]))
  ;; ignore leading *. See discussion above.
  (define sequence
    (match (col-ref 'sequence line)
      [(regexp #px"\\*([0-9]+)" (list _ numstr))
       (assert (string->number (substring (assert numstr string?) 1)) nat?)]
      [(regexp #px"[0-9]+" (list numstr))
       (assert (string->number numstr) nat?)]
      [other  (raise-argument-error 'line->sequence
                                    "sequence field containing number"
                                    line)]))
  (define ttf
    (match (col-ref 'team-teaching-frac line)
      ["" 0.0]
      [other (match (string->number other)
               [(? nonnegative-real? n) n]
               [other (raise-argument-error
                       'line->sequence
                       "team-teaching-frac field containing nonnegative real"
                       line)])]))
  (OfferingSequence (col-ref 'subject line)
                    (col-ref 'course-num line)
                    section
                    (col-ref 'instructor line)
                    sequence
                    (col-ref 'days line)
                    (col-ref 'time-start line)
                    (col-ref 'time-stop line)
                    (string->number/0 (col-ref 'tba-hours line))
                    (col-ref 'facility line)
                    (col-ref 'space line)
                    (col-ref 'facility-type line)
                    ttf))

(: normalize-level : (String -> Level))
(define (normalize-level level)
  (match level
    ["3" "GD"]
    ["2" "UD"]
    ["1" "LD"]
    ["GD" "GD"]
    ["UD" "UD"]
    ["LD" "LD"]))

;; ensure all values are the same, return it.
(: allthesame ((Listof AssocLine) Symbol -> String))
(define (allthesame lines column)  
  (define vals (map (λ ([line : AssocLine]) (col-ref column line)) lines))
  (match (remove-duplicates vals)
    [(list v) v]
    [other (raise-argument-error 'allthesame
                                 (format "list of identical values in column ~e"
                                         column)
                                 0 lines column)]))

;; ensure there is at most one non-blank value, return it.
(: atmostone ((Listof AssocLine) Symbol -> (U #f String)))
(define (atmostone lines column)
  (define vals (map (λ ([line : AssocLine]) (col-ref column line)) lines))
  (define nonblankvals (filter (λ ([s : String]) (not (equal? s ""))) vals))
  (match (remove-duplicates nonblankvals)
    [(list) #f]
    [(list v) v]
    [other (raise-argument-error 'atmostone
                                 (format "list containing one nonzero nonblank value in column ~e"
                                         column)
                                 0 lines column)]))


;; collapse classifications. Discard zeros and blanks, hope for
;; a remaining value, return 0 if not.
(: collapse-classification ((Listof AssocLine) -> (U #f Natural)))
(define (collapse-classification lines)
  (define vals (map (λ ([line : AssocLine]) (col-ref 'classification line)) lines))
  (define nonblanknonzerovals (filter (λ ([s : String]) (and (not (equal? s ""))
                                                             (not (equal? s "00"))))
                                      vals))
  (match (remove-duplicates nonblanknonzerovals)
    [(list) #f]
    [(list v) (assert (string->number v) nat?)]
    [other (raise-argument-error 'collapse-classifications
                                 "at most one nonzero nonblank value for classification"
                                 0 lines)]))

;; treat every space as a number, add them together
(: sum-of-nums ((Listof AssocLine) Symbol -> Real))
(define (sum-of-nums lines column)
  (define vals (map (λ ([line : AssocLine]) (col-ref column line)) lines))
  (define nonblankvals (filter (λ ([s : String]) (not (equal? s ""))) vals))
  (apply + (map (λ ([s : String]) (assert (string->number s) real?)) nonblankvals)))

;; given a list of lines representing a section, compute the sum of the
;; given field for each instructor, ensure that they're all the same,
;; and then return that number
(: sum-by-instructor/allthesame ((Listof AssocLine) Symbol -> Real))
(define (sum-by-instructor/allthesame lines field)
  (define instructor-groups
    (group-by (λ ([l : AssocLine]) (col-ref 'instructor l)) lines))
  (define instructor-sums
    (for/list : (Listof Real) ([g (in-list instructor-groups)])
      (sum-of-nums g field)))
  (define unique-sums (remove-duplicates instructor-sums))
  (unless (= 1 (length unique-sums))
    (error 'sum-by-instructor/allthesame
           "expected each instructor to have the same sum for field ~e, got: ~e"
           field
           instructor-sums))
  (first unique-sums))


;; (dept-pages format -> (listof instructorlines)
(: dept->instructors (dept-pages Format -> (Listof InstructorLines)))
(define (dept->instructors dept fformat)
  (match fformat
    ['pre-2144
     (define instructor-sets
       (pre-2144-dept-lines-parser
        (dept-pages-detail dept) (dept-pages-name dept)))
     (filter InstructorLines-home?
             (map pre-2144-parse-instructor instructor-sets))]
    [(or 'post-2142 'post-2164 '2174-fmt)
     (define instructor-sets
       (post-2142-dept-lines-parser
        (dept-pages-detail dept) (dept-pages-name dept)))
     (map (post-2142-parse-instructor fformat) instructor-sets)]))

;; these fields can occur only once, in the top line.
(define once-only-topline-fields
  (list->set
   '(subject
     course-num 
     section 
     discipline 
     level 
     enrollment
     #;a-ccu
     group-code
     #;classification)))


;; these fields can occur only once, in the starred line
(define once-only-someline-fields
  (list->set 
   '(direct-wtu
     indirect-wtu
     faculty-contact-hours
     scu
     total-wtu)))


;; parse a set of instructor lines
(: pre-2144-parse-instructor
   ((List (Listof String) String) -> InstructorLines))
(define (pre-2144-parse-instructor instructor-lines)
  (match-define (list opening-lines total-line) instructor-lines)
  (define header-line 
    (parse-instructor-header-line (first opening-lines) 'pre-2144))
  (define course-lines
    (map (parse-course-line 'pre-2144) (rest opening-lines)))
  #;(define combined-course-lines
    (regroup-sections course-lines 'pre-2144))
  (define summary-line (parse-summary-line total-line 'pre-2144))
  (InstructorLines
   header-line
   summary-line
   course-lines
   (home-record? header-line
                 summary-line
                 course-lines)))

(: post-2142-parse-instructor
   (Format
    ->
    ((U (List 'split-appt-remote String (Listof String) String)
        (List 'home-dept String (Listof String) (Listof String) String)
        (List 'no-class-instructor String (Listof String) String))
     -> InstructorLines)))
(define ((post-2142-parse-instructor fformat) instructor-lines)
  
  (match instructor-lines
    [(list 'split-appt-remote header-line specials summary-line)
     (InstructorLines
      (parse-instructor-header-line header-line fformat)
      (parse-summary-line summary-line fformat)
      (map (parse-course-line fformat) specials)
      #f)]
    [(list 'home-dept header-line specials regulars summary-line)
     (InstructorLines
      (parse-instructor-header-line header-line fformat)
      (parse-summary-line summary-line fformat)
      (append (map (parse-course-line fformat) specials)
              (map (parse-course-line fformat) regulars))
      #t)]
    [(list 'no-class-instructor header-line specials summary-line)
     (InstructorLines
      (parse-instructor-header-line header-line fformat)
      (parse-summary-line summary-line fformat)
      (map (parse-course-line fformat) specials)
      #t)]))

;; strip the information about courses out to transform an InstructorLines
;; into an instructor
(: lines->instructor (InstructorLines -> Instructor))
(define (lines->instructor ilines)
  (Instructor (InstructorLines-header ilines)
              (InstructorLines-summary ilines)
              (map line->special (filter
                                  special?
                                  (InstructorLines-lines ilines)))
              (InstructorLines-home? ilines)))

;; transform a special line into a Special structure
(: line->special (KindAssocLine -> Special))
(define (line->special l)
  (Special (col-ref 'special (rest l))
           (string->number/0 (col-ref 'scu (rest l)))
           (string->number/0 (col-ref 'faculty-contact-hours (rest l)))
           (string->number/0 (col-ref 'direct-wtu (rest l)))
           (string->number/0 (col-ref 'indirect-wtu (rest l)))))

;; extract the courses from an instructor, associating the
;; instructor name with each. ORDER IS SIGNIFICANT.
(: instructor-courses
   (InstructorLines -> (Listof AssocLine)))
(define (instructor-courses instr)
  (define name (col-ref 'name (rest (InstructorLines-header instr))))
  (map (attach-name name)
       (map (inst cdr Any AssocLine)
            (filter (λ ([c : KindAssocLine]) (not (special? c)))
                    (InstructorLines-lines instr)))))

;; attach the given name to each line
(: attach-name (String -> (AssocLine -> AssocLine)))
(define (attach-name name)
  (λ ([line : AssocLine]) (cons (list 'instructor name) line)))

;; copy subject/coursenum/section info from prior courses
(: attach-course-identity-info
   ((Listof AssocLine) -> (Listof AssocLine)))
(define (attach-course-identity-info lines)
  (cond [(empty? lines) '()]
        [else
         (define starting-id (course-line-id (first lines)))
         (when (blank-id? starting-id)
           (raise-argument-error 'attach-course-identity-info
                                 "list of lines with 1st non-blank id"
                                 0 lines))
         (let loop ([lines lines]
                    [prev-id : (List String String String) starting-id])
           (cond [(empty? lines) '()]
                 [else
                  (cond [(blank-id? (course-line-id (first lines)))
                         (cons (add-id prev-id (first lines))
                               (loop (rest lines) prev-id))]
                        [else
                         (cons (first lines)
                               (loop (rest lines)
                                     (course-line-id (first lines))))])]))])
  )


;; group the course lines by subj/num/sect
(: regroup-lines ((Listof AssocLine) -> (Listof (Listof AssocLine))))
(define (regroup-lines lines)
  (group-by
   course-line-id
   lines))

;; return the subject/num/section for a courseline
(: course-line-id (AssocLine -> (List String String String)))
(define (course-line-id line)
  (list (col-ref 'subject line)
        (col-ref 'course-num line)
        (col-ref 'section line)))

;; is this id blank? signal an error if only some are blank
(: blank-id? ((List String String String) -> Boolean))
(define (blank-id? id)
  (cond [(andmap (λ ([s : String]) (equal? s "")) id) #t]
        [(ormap (λ ([s : String]) (equal? s "")) id)
         (raise-argument-error 'blank-id?
                               "all blank or all non-blank strings"
                               0 id)]
        [else #f]))

;; replace the (blank) id elements
(: add-id ((List String String String) AssocLine -> AssocLine))
(define (add-id id line)
  (match-define (list subject course-num section) id)
  (append `((subject ,subject)
            (course-num ,course-num)
            (section ,section))
          (filter (λ ([l : (List Symbol Any)])
                    (not (member (first l) '(subject course-num section))))
                  line)))


;; okay, this is nasty. In pre-2144, the classes taught by split
;; appointment faculty appear in both their home department and
;; their other appointment, it's just that in their non-home
;; department, the bottom-line totals are uniformly zero. blecch.
;; instructor -> boolean
(: home-record? (AssocLine AssocLine (Listof KindAssocLine) -> Boolean))
(define (home-record? header summary course-lines)
  (define instructor-name (col-ref 'name header))
  ;; is this a home record for this column?
  (define col-results
    (for/list : (Listof Symbol)
        ;; TEMPORARILY OMITTING INDIRECT WTU! WAITING TO HEAR FROM CSU ON PLACEMENT IN FAD
        ([col (in-list '(scu faculty-contact-hours direct-wtu #;indirect-wtu))])
      (define given-total (string->number/0 (col-ref col summary)))
      (define computed-total
        (for/sum : Real ([l course-lines])
          ;; in pre-2144, this shouldn't be a list...
          (string->number/0
           (assert (col-ref col (rest l)) string?))))
      ;; sadly, there are two kinds of rounding error here:
      (cond [(and (small? given-total 1e-4)
                  (small? computed-total 1e-4)) 'both]
            [(small? (- given-total computed-total) 
                    (+ 1e-10 (* 0.1 (length course-lines))))
             'correct-nonzero]
            [(small? given-total 1e-4) 'artificial-zero]
            [else
             (error 'home-record
                    (~a "column "col" doesn't add up for "
                        instructor-name". computed "
                        computed-total", saw "given-total"\ncourses:"
                        course-lines"\n"))])))
  (cond [(no-artificial-zeros? col-results) #t]
        [(no-correct-nonzeros? col-results) #f]
        [else 
         (error 'home-record
                (~a "expected record to be uniformly zero or to have correct sums, got: "
                    (~e (append col-results header course-lines summary))))]))


(: no-artificial-zeros? ((Listof Symbol) -> Boolean))
(define (no-artificial-zeros? col-results)
  (not (member 'artificial-zero col-results)))

(: no-correct-nonzeros? ((Listof Symbol) -> Boolean))
(define (no-correct-nonzeros? col-results)
  (not (member 'correct-nonzero col-results)))

(: small? (Real Real -> Boolean))
(define (small? a tolerance) (<= (abs a) tolerance))

(: string->number/0 (String -> Nonnegative-Real))
(define (string->number/0 str)
  (cond [(string=? str "") 0.0]
        [else (assert (string->number str)
                      nonnegative-real?)]))




(: dept-short-name (String -> String))
(define (dept-short-name n)
  (match n
    [(regexp #px"DEPARTMENT - (\\d+) (.*)" (list _ num name))
     (dept-name-matcher (cast name String))]
    [(regexp #px"(\\d+) (.*)" (list _ num name))
     (dept-name-matcher (cast name String))]
    [other
     (raise-argument-error 'dept-short-name "department name matching pattern" 0 n)]))


;; shorten department names
(: dept-name-matcher (String -> String))
(define (dept-name-matcher name)
  (match name
    ["AERO ENG" "AERO"]
    ["ALL SCHOOL" "ALLSCHOOL"]
    ["CIVIL/ENV ENG" "CEENVE"]
    ;; change to CSSE?
    ["COMPUTER SCIENCE" "CSC"]
    ["BIOMEDICAL ENGINEERING" "BMGE"]
    ["ELECTRICAL ENGINEERING" "EE"]
    ["IND ENG" "IME"]
    ["MECHANICAL ENG" "ME"]
    ["WELDING AND METALLURGICAL ENGINEERING" "MATE"]))



;; does this line describe a starred sequence?
(: starred-sequence-line? (KindAssocLine -> Boolean))
(define (starred-sequence-line? l)
  (regexp-match? #px"^\\*" (col-ref 'sequence (cdr l))))

(: line-kind ((Pairof (List 'kind Symbol) Any) -> Symbol))
(define (line-kind line)
  (cadr (car line)))

(: col-ref (Symbol AssocLine -> String))
(define (col-ref title record)
  (match (assoc title record)
    [#f (error 'col-ref
               (~a "expected record with field '"title", got: "(~e record)))]
    [(list _ val) val]))

(: cols-ref ((Listof Symbol) KindAssocLine -> (Listof String)))
(define (cols-ref titles record)
  (map (λ ([title : Symbol]) (col-ref title (cdr record))) titles))


(: special? (KindAssocLine -> Boolean))
(define (special? course)
  (eq? (line-kind course) 'special))


;; short name for convenience
(define nat? exact-nonnegative-integer?)


(define-predicate nonnegative-real? Nonnegative-Real)


(module+ test
  (require typed/rackunit)

 )

