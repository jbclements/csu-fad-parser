#lang typed/racket

;; these functions break up lines into association lists, based
;; on known formats (column divisions)

(define-type Format (U 'pre-2144 'post-2142))
(define-type AssocLine
  (Listof (List Symbol String)))
(define-type BodyLine (U CourseLine SpecialLine))
(define-type CourseLine
  (Pairof (List 'kind (U 'class 'extra-sequence))
          (Listof (List Symbol String))))
(define-type SpecialLine
  (List (List 'kind 'special)
        (List 'special String)
        (List 'scu String)
        (List 'faculty-contact-hours String)
        (List 'direct-wtu String)
        (List 'indirect-wtu String)))

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
(define (parse-instructor-header-line line format)
  (string-split-cols instructor-header-split-info line format))

;; parse a course line
(: parse-course-line (Format -> (String -> BodyLine)))
(define ((parse-course-line format) line)
  (cond [(or
          (and (eq? format 'pre-2144) (starts-with-blanks line 38))
          (and (eq? format 'post-2142) (starts-with-blanks line 34)))
         ;; special section number
         (cast
          (cons '(kind extra-sequence)
                (string-split-cols course-info-split-info line format))
          CourseLine)]
        ;; non-course-thing
        [(or (and (eq? format 'pre-2144) (starts-with-blanks line 10))
             (and (eq? format 'post-2142) (starts-with-blanks line 8)))
         (cast
          (cons '(kind special)
               (string-split-cols special-stuff-split-info line format))
          SpecialLine)]
        [else
         (cast
          (cons '(kind class)
                (string-split-cols course-info-split-info line format))
          CourseLine)]))

;; parse a summary line
(: parse-summary-line (String Format -> AssocLine))
(define (parse-summary-line line format)
  (string-split-cols instructor-summary-split-info line format))


;; does this string start with 'n' whitespace characters?
(: starts-with-blanks (String Natural -> Boolean))
(define (starts-with-blanks str n)
  (regexp-match? #px"^\\s*$" (substring str 0 n)))


(define-type SplitInfo (Listof SplitInfoField))
(define-type SplitInfoField (List (List Natural Natural)
                                  FmtLabel
                                  Symbol))

(define-type FmtLabel (U 'id 'alpha 'alphanum 'alphanum= 'nums 'course-num
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
    ['decimal #px"^[\\d]*\\.[\\d]+$"]))



;; given a split-info and a string and a format selector,
;; return an association list of the form (Listof (List Symbol String))
(: string-split-cols (SplitInfo String Format
                                -> (Listof (List Symbol String))))
(define (string-split-cols split-info str format)
  (: posn-fun ((List Natural Natural) -> Natural))
  (define posn-fun
    (match format
      ['pre-2144 first]
      ['post-2142 second]))
  (define len (string-length str))
  (define full-cols-list
    (append
     (map posn-fun
          (map (inst first (List Natural Natural) Any)
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

(: instructor-header-split-info SplitInfo)
(define instructor-header-split-info
  '(((0 0) id id)
    ((13 10) alpha name) 
    ((41 38) alpha rank) 
    ((63 59) decimal tsf)
    ((68 65) decimal iaf) 
    ((80 71) alphanum adm-lvl)
    ((93 86) decimal osf)
    ((101 91) alphanum split) 
    ((124 116) alphanum= split-frac)
    ((130 127) decimal iff)))

(: course-info-split-info SplitInfo)
(define course-info-split-info
  '(((0 0) alpha dept)
    ((12 7) course-num course-num)
    ((22 12) nums section)
    ((26 19) nums discipline)
    ((31 25) alphanum level)
    ((34 29) nums enrollment)
    ((38 33) seq-num sequence)
    ((43 37) nums classification)
    ((46 40) decimal a-ccu)
    ((51 46) alpha days)
    ((58 52) nums time-start)
    ((63 57) nums time-stop)
    ((68 62) decimal tba-hours)
    ((75 68) alphanum facility)
    ((79 73) alphanum space)
    ((86 79) alphanum facility-type)
    ((90 84) nums group-code)
    ((95 89) decimal team-teaching-frac)
    ((101 95) decimal scu)
    ((109 102) decimal faculty-contact-hours)
    ((114 109) decimal direct-wtu)
    ((121 116) decimal indirect-wtu)
    ((127 123) decimal total-wtu)))

(: special-stuff-split-info SplitInfo)
(define special-stuff-split-info
  '(((0 0) alpha special)
    ((101 95) decimal scu)
    ((109 102) decimal faculty-contact-hours)
    ((114 109) decimal direct-wtu)
    ((121 116) decimal indirect-wtu)))

(: instructor-summary-split-info SplitInfo)
(define instructor-summary-split-info
  '(((0 0) alpha total-individual)
    ((34 28) nums enrolled)
    ((101 94) decimal scu)
    ((109 101) decimal faculty-contact-hours)
    ((114 107) decimal direct-wtu)
    ((121 114) decimal indirect-wtu)
    ((127 121) decimal total-wtu)))

(module+ test

  (require typed/rackunit)

  (check-equal?
   (string-split-cols
    instructor-summary-split-info
    " TOTAL INDIVIDUAL             71                                                                273.2  13.6   11.0    0.0   11.0"
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
    instructor-header-split-info
    " XXXXX5705    E I ELGHANDOUR          ASSOC PROF/LECT C    1.000  0.000               0.000  SPLIT APPT 52176       IFF=0.467   0.533"
    'post-2142)
   '((id "XXXXX5705")
     (name "E I ELGHANDOUR")
     (rank "ASSOC PROF/LECT C")
     (tsf "1.000")
     (iaf "0.000" )
     (adm-lvl "")
     (osf "0.000")
     (split "SPLIT APPT 52176")
     (split-frac "IFF=0.467")
     (iff "0.533"))
   ))

