#lang typed/racket

;; these functions break up lines into association lists, based
;; on known formats (column divisions)

(define-type Format (U 'pre-2144 'post-2142 'post-2164 '2174-fmt))
(define-type AssocLine
  (Listof (List Symbol String)))
(define-type BodyLine (U CourseLine SpecialLine))
(define-type CourseLine
  (Pairof (List 'kind (U 'class 'extra-sequence))
          (Listof (List Symbol String))))
(define-type SpecialLine
  (Pairof (List 'kind 'special)
          (Listof (List Symbol String))))

(provide parse-instructor-header-line
         parse-course-line
         parse-summary-line
         AssocLine
         Format)

#;(struct instructor ([header : LineAssoc]
                    [summary : ]
                    [courses : (Listof )]) #:transparent)


;; parse an instructor header line
(: parse-instructor-header-line (String Format -> AssocLine))
(define (parse-instructor-header-line line fformat)
  (match fformat
    ['2174-fmt
     (string-split-cols-2 2174-instructor-splitinfo line)]
    [_
     (string-split-cols instructor-header-split-info line fformat)]))

;; how many blanks are required for an extra-sequence line?
(define (extra-sequence-blanks fmt) : Natural
  (match fmt
    ['pre-2144 38]
    [_         34]))

;; how many blanks are required for a special?
(define (special-blanks fmt) : Natural
  (match fmt
    ['pre-2144 10]
    [_         8]))

;; parse a course line
(: parse-course-line (Format -> (String -> BodyLine)))
(define ((parse-course-line fformat) line)
  (cond [(starts-with-blanks line (extra-sequence-blanks fformat))
         ;; special section number
         ;; gross format splitting, mid-refactor...
         (cast
          (cons
           '(kind extra-sequence)
           (match fformat
             ['2174-fmt
              (string-split-cols-2
               2174-course-splitinfo
               line)]
             [_
              (string-split-cols course-info-split-info line fformat)]))
          CourseLine)]
        ;; non-course-thing
        [(starts-with-blanks line (special-blanks fformat))
         (cast
          (cons '(kind special)
                (match fformat
                  ['2174-fmt
                   (string-split-cols-2 2174-special-splitinfo line)]
                  [_
                   (string-split-cols special-stuff-split-info line fformat)]))
          SpecialLine)]
        [else
         ;; this is gross... mid-refactoring here
         (cast
          (cons '(kind class)
                (match fformat
           ['2174-fmt
            (string-split-cols-2
             2174-course-splitinfo
             line)]
           [other
            (string-split-cols course-info-split-info line fformat)]
         ))
         CourseLine)]))

;; parse a summary line
(: parse-summary-line (String Format -> AssocLine))
(define (parse-summary-line line fformat)
  (match fformat
    ['2174-fmt
     (string-split-cols-2 2174-instructor-summary-splitinfo line)]
    [_
     (string-split-cols instructor-summary-split-info line fformat)]))


;; does this string start with 'n' whitespace characters?
(: starts-with-blanks (String Natural -> Boolean))
(define (starts-with-blanks str n)
  (regexp-match? #px"^\\s*$" (substring str 0 n)))


(define-type SplitInfo (Listof SplitInfoField))
(define-type SplitInfoField (List (List Natural Natural Natural Natural)
                                  FmtLabel
                                  Symbol))
;; new style. Replace old style?
(define-type SplitInfo2 (Listof SplitInfoField2))
(define-type SplitInfoField2
  (List Natural
        FmtLabel
        Symbol))

(define-type FmtLabel (U 'id 'alpha 'alphanum 'alphanum= 'nums
                         'course-num 'course-id
                         'seq-num 'decimal))

(: pat (FmtLabel -> Regexp))
(define (pat sym)
  (match sym
    ['id #px"^(0 |)XXXXX\\d\\d\\d\\d$"]
    ['alpha #px"^[-A-Z'/ ]*$"]
    ['alphanum #px"^[-A-Z'/ 0-9.]*$"]
    ['alphanum= #px"^[-A-Z'/ 0-9=.]*$"]
    ['nums #px"^\\d+$"]
    ['course-num #px"^\\d\\d\\d\\d( X)?$"]
    ['seq-num #px"^(\\*|)\\d+$"]
    ['decimal #px"^[\\d]*\\.[\\d]+$"]
    ['course-id #px"^[A-Z]{2,4} +[0-9]{4}$"]))



;; given a split-info and a string and a format selector,
;; return an association list of the form (Listof (List Symbol String))
(: string-split-cols (SplitInfo String Format
                                -> (Listof (List Symbol String))))
(define (string-split-cols split-info str format)
  (: posn-fun ((List Natural Natural Natural Natural) -> Natural))
  (define posn-fun
    (match format
      ['pre-2144 first]
      ['post-2142 second]
      ['post-2164 third]
      ['2174-fmt fourth]))
  (define len (string-length str))
  (define full-cols-list
    (append
     (map posn-fun
          (map (inst first (List Natural Natural Natural Natural) Any)
               split-info))
     (list (string-length str))))
  (for/list ([start full-cols-list]
             [stop (rest full-cols-list)]
             [pat-sym : FmtLabel
                      (map (ann second (SplitInfoField -> FmtLabel))
                           split-info)]
             [field-name (map (ann third (SplitInfoField -> Symbol))
                              split-info)])
    (define substr (string-trim (substring str (min start len) (min stop len))))
    (unless (or (string=? "" substr) (regexp-match (pat pat-sym) substr))
      (error 'split-string-cols 
             (~a "expected field matching pattern "(~s pat-sym)", got: "
                 (~s substr)
                 "in line "(~s str))))
    (list field-name substr)))

;; given a split-info-2 and a string,
;; return an association list of the form (Listof (List Symbol String))
;; ...I'd like to replace string-split-cols with string-split-cols-2
;; everywhere.
(: string-split-cols-2 (SplitInfo2 String
                                 -> (Listof (List Symbol String))))
(define (string-split-cols-2 split-info str)
  (define len (string-length str))
  (define full-cols-list
    (append
     (map (inst first Natural Any)
          split-info)
     (list (string-length str))))
  (for/list ([start full-cols-list]
             [stop (rest full-cols-list)]
             [pat-sym : FmtLabel
                      (map (ann second (SplitInfoField2 -> FmtLabel))
                           split-info)]
             [field-name (map (ann third (SplitInfoField2 -> Symbol))
                              split-info)])
    (define substr (string-trim (substring str (min start len) (min stop len))))
    (unless (or (string=? "" substr) (regexp-match (pat pat-sym) substr))
      (error 'split-string-cols 
             (~a "expected field matching pattern "(~s pat-sym)", got: "
                 (~s substr)
                 "in line "(~s str))))
    (list field-name substr)))

;; I'm now using a template-based approach to derive the column
;; offsets. I haven't yet applied this to FADs prior to 2174.
;; If all of them are moved to the template approach, these can
;; be deleted.
(: instructor-header-split-info SplitInfo)
(define instructor-header-split-info
  '(((0 0 0 0) id id)
    ((13 10 12 11) nums other-id)
    ((13 10 29 28) alpha name)
    ((41 38 53 51) alpha rank)
    ((63 59 74 71) decimal tsf)
    ((68 65 81 78) decimal iaf) 
    ((80 71 88 85) alphanum adm-lvl)
    ((93 86 106 102) decimal osf)
    ((101 91 113 109) alphanum split) 
    ((124 116 136 131) alphanum= split-frac)
    ((130 127 148 142) decimal iff)))

(: course-info-split-info SplitInfo)
(define course-info-split-info
  '(((0 0 0 0) alpha subject)
    ((12 7 7 7) course-num course-num)
    ((22 12 12 14) nums section)
    ((26 19 19 18) nums discipline)
    ((31 25 25 24) alphanum level)
    ((34 29 29 27) nums enrollment)
    ((38 33 33 33) seq-num sequence)
    ((43 37 37 36) nums classification)
    ((46 40 40 39) decimal a-ccu)
    ((51 46 46 44) alpha days)
    ((58 52 52 50) nums time-start)
    ((63 57 57 55) nums time-stop)
    ((68 62 62 60) decimal tba-hours)
    ((75 68 68 65) alphanum facility)
    ((79 73 73 70) alphanum space)
    ((86 79 79 76) alphanum facility-type)
    ((90 84 84 81) nums group-code)
    ((95 89 89 85) decimal team-teaching-frac)
    ((101 95 95 92) decimal scu)
    ((109 102 102 99) decimal faculty-contact-hours)
    ((114 109 109 106) decimal direct-wtu)
    ((121 116 116 112) decimal indirect-wtu)
    ((127 123 123 118) decimal total-wtu)))


(: special-stuff-split-info SplitInfo)
(define special-stuff-split-info
  '(((0 0 0 0) alpha special)
    ((101 95 95 92) decimal scu)
    ((109 102 102 99) decimal faculty-contact-hours)
    ((114 109 109 106) decimal direct-wtu)
    ((121 116 116 112) decimal indirect-wtu)))

(: instructor-summary-split-info SplitInfo)
(define instructor-summary-split-info
  '(((0 0 0 0) alpha total-individual)
    ((34 28 28 27) nums enrolled)
    ((101 94 94 91) decimal scu)
    ((109 101 101 99) decimal faculty-contact-hours)
    ((114 107 107 105) decimal direct-wtu)
    ((121 114 114 112) decimal indirect-wtu)
    ((127 121 121 119) decimal total-wtu)))

;; let's try this a different way:

;; the first line shows where to split the columns, and the
;; second one provides the header names. Lines after the first
;; are directly copied from the FAD (ooh except for the bracketed
;; text I inserted for columns that have no header in the original)
;; (ooh and other doctoring to split department and number...)
(define 2174-class-template
#<<|
      X    X      X     X   X   X   X  X     X     X    X    X    X     X     X    X   X      X      X     X      X      X      
  SUBJ NUM    SECT HEGIS LVL ENR  LS CS A-CCU DAYS  BEG  END   TBA  FACL SPACE TYPE GRP   TTF    SCU    FCH  D-WTU  I-WTU  T-WTU
  AERO 0300    05  09021 UD   27  10 16  1.0  W     1510 1800  0.0  192  0321  LAB       1.000   27.0   2.8    2.0
  AERO 0307    02  09021 UD   18  10 16  2.0  TTH   1210 1500  0.0  041  0136  LAB       1.000   36.0   5.8    4.0
  CSC  0445    01  07011 UD   30  10 04  4.0  MTWTH 0810 0900  0.0  014  0253  LECT      1.000  120.0   3.3    4.0
  AERO 0299    03  09021 LD   22  10 16  1.0  M     1510 1800  0.0  041  0144  LAB       1.000   22.0   2.8    2.0
  AERO 0299    06  09021 LD   22  10 16  1.0  W     1510 1800  0.0  041  0144  LAB       1.000   22.0   2.8    2.0
|
)

;; for each named class offering header field, provides the format and symbol name
(define class-header-to-field-info-map : (Listof (List String Symbol Symbol))
  '(("SUBJ" alpha subject)
    ("NUM" course-num course-num)
    ("SECT" nums section)
    ("HEGIS" nums discipline)
    ("LVL" alphanum level)
    ("ENR" nums enrollment)
    ("LS" seq-num sequence)
    ("CS" nums classification)
    ("A-CCU" decimal a-ccu)
    ("DAYS" alpha days)
    ("BEG" nums time-start)
    ("END" nums time-stop)
    ("TBA" decimal tba-hours)
    ("FACL" alphanum facility)
    ("SPACE" alphanum space)
    ("TYPE" alphanum facility-type)
    ("GRP" nums group-code)
    ("TTF" decimal team-teaching-frac)
    ("SCU" decimal scu)
    ("FCH" decimal faculty-contact-hours)
    ("D-WTU" decimal direct-wtu)
    ("I-WTU" decimal indirect-wtu)
    ("T-WTU" decimal total-wtu)))

(define 2174-instructor-template
  #<<|
          X                X                        X                    X      X     X                  X     X                   X               X
   SSN        EMPLOYEE ID     NAME                   RANGE CODE            TSF    IAF     ADM-LVL           OSF        <split>       <splitfrac>     IFF
        ASSIGNED TIME ACTIVITY
                                                                           TSF    IAF                       OSF                                      IFF
 XXXXX4730  000000007319825  B J BULDIVON            TCHNG ASSOCIATE      0.400  0.000                    0.000                                     0.400
 XXXXX8023  000000000819101  D D MARTHALL            PROFESSOR/LECT D     1.000  0.780  DEPT ACADEMIC YR  0.000                                     0.220
 XXXXX5340  000000004795260  M A ISOLA               INSTRUCTOR/LECT A    0.804  0.000                    0.000  SPLIT APPT 20108       IFF=0.267   0.537
 XXXXX1234  000000000001234  J P MEATHER             ADMINISTRATOR        1.000  1.000  CAMPUS DEAN/CHAIR 0.000                                     0.000
|
  )

(define instructor-header-to-field-info-map : (Listof (List String Symbol Symbol))
  '(("SSN" id id) ;; 'id is a TERRIBLE name for this. they can collide!
    ("EMPLOYEE ID" nums other-id)
    ("NAME" alpha name)
    ("RANGE CODE" alpha rank)
    ("TSF" decimal tsf)
    ("IAF" decimal iaf) 
    ("ADM-LVL" alphanum adm-lvl)
    ("OSF" decimal osf)
    ("<split>" alphanum split) 
    ("<splitfrac>" alphanum= split-frac)
    ("IFF" decimal iff)))

(define 2174-instructor-summary-template
  #<<|
                            X                                                                  X     X     X      X      X
 TITLE                       ENR                                                                 SCU    FCH  D-WTU  I-WTU  T-WTU
 TOTAL INDIVIDUAL             55                                                                 81.3  14.9    8.1    1.0    9.1
|
  )

(define instructor-summary-header-to-field-info-map : (Listof (List String Symbol Symbol))
  '(("TITLE" alpha total-individual)
    ("ENR" nums enrolled)
    ("SCU" decimal scu)
    ("FCH" decimal faculty-contact-hours)
    ("D-WTU" decimal direct-wtu)
    ("I-WTU" decimal indirect-wtu)
    ("T-WTU" decimal total-wtu)))

(define 2174-special-template
  #<<|
                                                                                                X    X     X      X       
        TITLE                                                                                     SCU  FCH   D-WTU  I-WTU
        EXCESS ENROLLMT                                                                                       4.0
        STUDENT ADVISING                                                                                              2.0
        INS EXPR OR INOV                                                                                              2.0
|
  )

(define special-header-to-field-info-map : (Listof (List String Symbol Symbol))
  '(("TITLE" alpha special)
    ("SCU" decimal scu)
    ("FCH" decimal faculty-contact-hours)
    ("D-WTU" decimal direct-wtu)
    ("I-WTU" decimal indirect-wtu)))

;; given 2-line template (Xs and header line), and a list mapping
;; header names to field information, return a splitinfo
(define (template->split-info
         [template-text : String]
         [class-header-to-field-info-map : (Listof (List String Symbol Symbol))]) : SplitInfo2
  (define template-lines (regexp-split #px"\n" template-text))
  (define x-line (first template-lines))
  (define header-row (second template-lines))
  (define x-columns
    (for/list : (Listof Natural)
      ([posn : Natural (in-naturals)]
       [char (in-string x-line)]
       #:when (equal? char #\X))
      posn))
  (define col-headers
    (for/list : (Listof String)
      ([start-posn (in-list (cons 0 x-columns))]
       [end-posn (in-list (append x-columns (list (string-length header-row))))])
      (string-trim (substring header-row start-posn end-posn))))

  (unless (equal? (list->set (map (inst first String Any)
                                  class-header-to-field-info-map))
                  (list->set col-headers))
    (error 'header-map
           "expected headers ~v (in any order), got headers ~v"
           (map (inst first String Any)
                class-header-to-field-info-map)
           col-headers))
  (for/list : (Listof (List Natural FmtLabel Symbol))
    ([col-header (in-list col-headers)]
     [start-posn (in-list (cons 0 x-columns))])
    (define field-info (match (assoc col-header class-header-to-field-info-map)
                         [#f (error 'header-map "impossible 2017-07")]
                         [other other]))
    (ann
     (list start-posn
           (cast (second field-info) FmtLabel)
           (third field-info))
     (List Natural FmtLabel Symbol))))

(define 2174-course-splitinfo : SplitInfo2
  (template->split-info 2174-class-template
                        class-header-to-field-info-map))

(define 2174-instructor-splitinfo : SplitInfo2
  (template->split-info 2174-instructor-template
                        instructor-header-to-field-info-map))

(define 2174-instructor-summary-splitinfo : SplitInfo2
  (template->split-info 2174-instructor-summary-template
                        instructor-summary-header-to-field-info-map))

(define 2174-special-splitinfo : SplitInfo2
  (template->split-info 2174-special-template
                        special-header-to-field-info-map))

(module+ test

  (require typed/rackunit)

  (check-equal?
   (string-split-cols
    instructor-summary-split-info
    " TOTAL INDIVIDUAL             71                    \
                                            \
273.2  13.6   11.0    0.0   11.0"
    'post-2142)
   '((total-individual "TOTAL INDIVIDUAL")
     (enrolled "71")
     (scu "273.2")
     (faculty-contact-hours "13.6")
     (direct-wtu "11.0")
     (indirect-wtu "0.0")
     (total-wtu "11.0")))

  (check-equal?
   (string-split-cols
    instructor-summary-split-info
    " TOTAL INDIVIDUAL            45                 \
                                             \
63.0   8.6    6.0   0.0    6.0"
    '2174-fmt)
   '((total-individual "TOTAL INDIVIDUAL")
     (enrolled "45")
     (scu "63.0")
     (faculty-contact-hours "8.6")
     (direct-wtu "6.0")
     (indirect-wtu "0.0")
     (total-wtu "6.0")))

  (check-equal?
   (string-split-cols
   instructor-summary-split-info
   " TOTAL INDIVIDUAL           397           \
                                                 \
1588.0  11.4   15.0   0.0   15.0"
   '2174-fmt)
   '((total-individual "TOTAL INDIVIDUAL")
     (enrolled "397")
     (scu "1588.0")
     (faculty-contact-hours "11.4")
     (direct-wtu "15.0")
     (indirect-wtu "0.0")
     (total-wtu "15.0")))

  (check-equal?
   (string-split-cols
    instructor-header-split-info
    " XXXXX5705    E I ELGHANDOUR          ASSOC PROF/LECT C    1.000  0.000               0.000  SPLIT APPT 52176       IFF=0.467   0.533"
    'post-2142)
   '((id "XXXXX5705")
     (other-id "")
     (name "E I ELGHANDOUR")
     (rank "ASSOC PROF/LECT C")
     (tsf "1.000")
     (iaf "0.000" )
     (adm-lvl "")
     (osf "0.000")
     (split "SPLIT APPT 52176")
     (split-frac "IFF=0.467")
     (iff "0.533"))
   )

  (check-equal?
   (string-split-cols
    instructor-header-split-info
    " XXXXX8231  000000000017182  K T DEVANEY             \
PROFESSOR/LECT D     0.867  0.000                    0.000  \
SPLIT APPT 20108       IFF=0.200   0.667"
    'post-2164)
   '((id "XXXXX8231")
     (other-id "000000000017182")
     (name "K T DEVANEY")
     (rank "PROFESSOR/LECT D")
     (tsf "0.867")
     (iaf "0.000" )
     (adm-lvl "")
     (osf "0.000")
     (split "SPLIT APPT 20108")
     (split-frac "IFF=0.200")
     (iff "0.667"))
   )

  (check-equal?
   (string-split-cols
    instructor-header-split-info
    " XXXXX9533  000000001284877 M A ESOLA              \
INSTRUCTOR/LECT A   0.804  0.000                   0.000  \
SPLIT APPT 20108      IFF=0.267  0.537"
    '2174-fmt)   
   '((id "XXXXX9533")
     (other-id "000000001284877")
     (name "M A ESOLA")
     (rank "INSTRUCTOR/LECT A")
     (tsf "0.804")
     (iaf "0.000" )
     (adm-lvl "")
     (osf "0.000")
     (split "SPLIT APPT 20108")
     (split-frac "IFF=0.267")
     (iff "0.537")))

  (check-equal?
   (string-split-cols
    course-info-split-info
    "  AERO 0300   05  09021 UD  227  10 16 1.0  W     \
1510 1800  0.0 192  0321  LAB      1.000  827.0   2.8    \
2.0"
    '2174-fmt)
   '((subject "AERO")
  (course-num "0300")
  (section "05")
  (discipline "09021")
  (level "UD")
  (enrollment "227")
  (sequence "10")
  (classification "16")
  (a-ccu "1.0")
  (days "W")
  (time-start "1510")
  (time-stop "1800")
  (tba-hours "0.0")
  (facility "192")
  (space "0321")
  (facility-type "LAB")
  (group-code "")
  (team-teaching-frac "1.000")
  (scu "827.0")
  (faculty-contact-hours "2.8")
  (direct-wtu "2.0")
  (indirect-wtu "")
  (total-wtu ""))
   )


  (check-equal?
   (string-split-cols
    special-stuff-split-info
    "        INS EXPR OR INOV                        \
                                                                 2.0"

    '2174-fmt)
   '((special "INS EXPR OR INOV")
     (scu "")
     (faculty-contact-hours "")
     (direct-wtu "")
     (indirect-wtu "2.0")))

  
  )
