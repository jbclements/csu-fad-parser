
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
         AssocLine)


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

(define-type AssocLine
  (Listof (List Symbol String)))