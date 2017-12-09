#lang racket


;; this is basically a scratchpad for generating code. I know, in
;; a perfect world, this would be done by a macro, eliminating
;; potential errors related to the alteration of these fields.

(define def
  '(struct FacultyOffering
     ([subject : String]
      [coursenum : String]
      [section : Natural]
      [instructor : String]
      [scu : Real]
      [contact-hours : Real]
      [dwtu : Real])
     #:prefab))

(define def2
  '(struct OfferingSequence
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
    #:prefab))

(define def3
  '(struct Instructor ([name : String]
                       [id : String]
                       [other-id : String]
                       [rank : Rank]
                       [adm-level : String]
                       [tsf : Real]
                       [iaf : Real]
                       [osf : Real]
                       [split-appt : (Listof (U String Natural))]
                       [split-frac : String]
                       [iff : Real]
                       (summary : AssocLine)
                       (specials : (Listof Special))
                       [home? : Boolean])
     #:prefab))

(define def4
  '(struct Special
     ([description : String]
      [scu : Nonnegative-Real]
      [contact-hours : Nonnegative-Real]
      [direct-wtu : Nonnegative-Real]
      [indirect-wtu : Nonnegative-Real])
     #:prefab))

(define (run-def def)
(define structname (second def))
  (cons 'list
(for/list ([fieldname (in-list (map first (third def)))])
  (list 'list
        (string->symbol (~a structname "-" fieldname))
        (list 'quote fieldname)))))

(run-def def4)