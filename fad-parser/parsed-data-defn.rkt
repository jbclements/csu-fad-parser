#lang typed/racket

;; this file contains the structures used in Typed Racket
;; to represent the Parsed FAD.

(provide (struct-out Instructor)
         (struct-out Dept)
         (struct-out Parsed)
         (struct-out Offering)
         (struct-out FacultyOffering)
         (struct-out OfferingSequence)
         (struct-out Special)
         Level
         Rank
         AdministrativeLevel
         AssocLine
         dept-number-mapping)


;;(make-parsed (listof (list string assoc-list)) (listof dept))
(struct Parsed ([college-summary : (U (Listof (List String (Listof String)))
                                      'no-college-summary-page)]
                [depts : (Listof Dept)]
                [offerings : (Listof Offering)]
                [faculty-offerings : (Listof FacultyOffering)]
                [sequences : (Listof OfferingSequence)])
  #:prefab)

;; represents a department's name, summary info, and instructors
;;(make-dept string (listof (list string assoc-list)) (listof instructor))
(struct Dept ([name : String]
              [summary : (Listof (List String (Listof String)))]
              [instructors : (Listof Instructor)])
  #:prefab)



;; represents information about an instructor, independent
;; of the courses he or she is teaching.
(struct Instructor ((header : AssocLine)
                    (summary : AssocLine)
                    (specials : (Listof Special))
                    [home? : Boolean])
  #:prefab)

;; an offering is a group of students signed up for a class.
(struct Offering
  ([subject : String]
   [coursenum : String]
   [section : Natural]
   [discipline : Natural]
   [level : Level]
   [enrollment : Natural]
   [classification : (U #f Natural)]
   [accu : Real]
   [groupcode : (U #f Natural)])
  #:prefab)

(define-type Level (U "LD" "UD" "GD"))

;; an offerfac describes an instructor's involvement with a section
(struct FacultyOffering
  ([subject : String]
   [coursenum : String]
   [section : Natural]
   [instructor : String]
   [scu : Real]
   [contact-hours : Real]
   [dwtu : Real])
  #:prefab)

;; an offerseq describes the information associated specifically with a
;; single sequence
(struct OfferingSequence
  ([subject : String]
   [coursenum : String]
   [section : Natural]
   [instructor : String]
   [sequence : Natural]
   [days : String]
   [time-start : String]
   [time-stop : String]
   [tba-hours : Nonnegative-Real]
   [facility : String]
   [space : String]
   [facility-type : String]
   [team-teach-frac : Nonnegative-Real])
  #:prefab)

;; represents a special assignment (such as course planning or
;; development)
(struct Special
  ([description : String]
   [scu : Nonnegative-Real]
   [contact-hours : Nonnegative-Real]
   [direct-wtu : Nonnegative-Real]
   [indirect-wtu : Nonnegative-Real])
  #:prefab)

;; represents a line that associates labels with strings
;; this is our representation of an arbitrary line taken
;; from a table-style text document that's broken up into
;; lines of data that are associated with column names.
(define-type AssocLine
  (Listof (List Symbol String)))

;; all ranks appearing in 2088 through 2178 after collapsing
;; of some lexical abbreviations.
(define-type Rank
  (U "ASSISTANT PRF/LECT B"
     "PROFESSOR/LECT D"
     "TEACHING ASSOCIATE"
     "ASSOCIATE PRF/LECT C"
     "ADMINISTRATOR"
     "INSTRUCTOR/LECT A"
     "TEACHING ASST/LECT L"
     "OTHER"))

;; all levels appearing in 2088 through 2178
(define-type AdministrativeLevel
  (U "" "DEPT ACAD YR" "SCHOOL OTHER" "CAMPUS DEAN"
     "DEPT 12 MO." "DEPT ACADEMIC YR" "SCHOOL A/A DEAN"
     "CAMPUS DEAN/CHAIR" "DEPT 12 MONTH"
     "CAMPUS OTHER APPT"))

;; a mapping between numbers and department names apparently
;; used for split appointments and in page headers.
;; this maybe shouldn't be in this file...
;; inferred from fad-2178
(define dept-number-mapping : (Listof (List Natural String))
  '((112 "AERO ENG")
    (132 "ALL SCHOOL")
    (176 "CIVIL/ENV ENG")
    (189 "COMPUTER SCIENCE")
    (224 "BIOMEDICAL ENGINEERING")
    (247 "ELECTRICAL ENGINEERING")
    (363 "IND ENG")
    (490 "MECHANICAL ENG")
    (770 "WELDING AND METALLURGICAL ENGINEERING")))