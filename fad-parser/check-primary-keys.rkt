#lang typed/racket

;; this file performs uniqueness checks of the kind that would
;; allow a "primary key" designation on database records.

(require "parsed-data-defn.rkt")

(provide check-offering-uniqueness
         check-offerfac-uniqueness
         check-offerseq-uniqueness)

(: check-uniqueness (All (T U) ((T -> U) -> ((Listof T) -> Void))))
(define ((check-uniqueness key-fun) elements)
  (define collisions : (Listof (Listof T))
    (filter (λ ([g : (Listof T)]) (< 1 (length g)))
          (group-by key-fun elements)))
  (unless (empty? collisions)
    (error 'check-uniqueness
           "multiple elements have the same key: ~e"
           collisions)))

(define check-offering-uniqueness
  (check-uniqueness
   (λ ([o : Offering]) : (List String String Natural)
     (list (Offering-subject o)
           (Offering-coursenum o)
           (Offering-section o)))))

(define check-offerfac-uniqueness
  (check-uniqueness
   (λ ([o : FacultyOffering])
     (list (FacultyOffering-subject o)
           (FacultyOffering-coursenum o)
           (FacultyOffering-section o)
           (FacultyOffering-instructor o)))))

(define check-offerseq-uniqueness
  (check-uniqueness
   (λ ([o : OfferingSequence])
     (list (OfferingSequence-subject o)
           (OfferingSequence-coursenum o)
           (OfferingSequence-section o)
           (OfferingSequence-instructor o)
           (OfferingSequence-sequence o)))))

(module+ test
  (require typed/rackunit)
  (check-exn #px"multiple elements have the same key"
             (λ ()
               (check-offering-uniqueness
                (list (Offering "WAGGA" "BAGGA" 3 1234 "UD" 4 #f 3.4 #f)
                      (Offering "WAGGA" "BuGGA" 3 1234 "UD" 4 #f 3.4 #f)
                      (Offering "WAGGA" "BAGGA" 3 1232434 "UD" 4 #f 3.4 #f)))))

  (check-equal? (check-offering-uniqueness
                 (list (Offering "WAGGA" "BAGGA" 3 1234 "UD" 4 #f 3.4 #f)
                       (Offering "WAGGA" "BuGGA" 3 1234 "UD" 4 #f 3.4 #f)
                       (Offering "WAGGA" "BAGGA" 9 1232434 "UD" 4 #f 3.4 #f)))
                (void)))

