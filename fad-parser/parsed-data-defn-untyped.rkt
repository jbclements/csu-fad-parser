
#lang racket

;; this file contains the structures used in Typed Racket
;; to represent the Parsed FAD.

;; this file is untyped. Apparently TR can't generate contracts
;; to protect prefab structures...

(provide (struct-out Instructor)
         (struct-out Dept)
         (struct-out Parsed)
         (struct-out Offering)
         (struct-out FacultyOffering)
         (struct-out OfferingSequence)
         (struct-out Special)
         dept-number-mapping
         )


;;(make-parsed (listof (list string assoc-list)) (listof dept))
(struct Parsed (college-summary
                depts
                offerings
                faculty-offerings
                sequences)
  #:prefab)

;; represents a department's name, summary info, and instructors
;;(make-dept string (listof (list string assoc-list)) (listof instructor))
(struct Dept (name
              summary
              instructors)
  #:prefab)



;; represents information about an instructor, independent
;; of the courses he or she is teaching.
(struct Instructor (header
                    summary
                    specials
                    home?)
  #:prefab)

;; an offering is a group of students signed up for a class.
(struct Offering
  (subject
   coursenum
   section
   discipline
   level
   enrollment
   classification
   accu
   groupcode)
  #:prefab)

;; an offerfac describes an instructor's involvement with a section
(struct FacultyOffering
  (subject
   coursenum
   section
   instructor
   scu
   contact-hours
   dwtu)
  #:prefab)

;; an offerseq describes the information associated specifically with a
;; single sequence
(struct OfferingSequence
  (subject
   coursenum
   section
   instructor
   sequence
   days
   time-start
   time-stop
   tba-hours
   facility
   space
   facility-type
   team-teach-frac)
  #:prefab)

;; represents a special assignment (such as course planning or
;; development)
(struct Special
  (description
   scu
   contact-hours
   direct-wtu
   indirect-wtu)
  #:prefab)

(define dept-number-mapping
  '((112 "AERO ENG")
    (132 "ALL SCHOOL")
    (176 "CIVIL/ENV ENG")
    (189 "COMPUTER SCIENCE")
    (224 "BIOMEDICAL ENGINEERING")
    (247 "ELECTRICAL ENGINEERING")
    (363 "IND ENG")
    (490 "MECHANICAL ENG")
    (770 "WELDING AND METALLURGICAL ENGINEERING")))