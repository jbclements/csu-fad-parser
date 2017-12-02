#lang racket


(define boxes
  '((offering
    (qtr subject num section discipline level enrollment classification
         a_ccu)
    (4 10))
    (offerfac
     (scu fch dwtu)
     (4 5))
    (sequence
      (ttf days start end tba facility space facltype)
      (4 0))
    (instructor
     ()
     (0 5))
    (status
     (iqtr home_dept tsf iaf osf admlevel rank)
     (0 10))
    (assigned
     (description a_scu contact_hours direct_wtu indirect_wtu)
     (0 0))
    (group
     ()
     #f))
  )

(define joiners
  '((offering has_group group)
    (offering consists_of offerfac)
    (offerfac containing sequence)
    (instructor teaches offerfac)
    (instructor has_status status)
    (instructor gets assigned)))

(define (generate-box box-spec)
  (match-define (list name attrs pos) box-spec)
  (define pos-str
    (cond [pos (~a ", pos = \""(first pos)", "(second pos)"!\"")]
          [else ""]))
  (cons
   (~a name " [ shape = box "pos-str"]")
   (apply
    append
    (for/list ([attr (in-list attrs)])
      (list (~a attr " [ shape = oval ] ")
            (~a name " -- " attr))))))

(define (generate-relationship relationship-spec)
  (match-define (list from relationship to) relationship-spec)
  (list (~a relationship " [ shape = diamond ] ")
        (~a from " -- " relationship)
        (~a relationship " -- " to)))


(define stmts
  (append
   (apply append (map generate-box boxes))
   (apply append (map generate-relationship joiners))))

(define lines
  (append
   (list "graph ER_Diagram { ")
   (for/list ([stmt (in-list stmts)])
     (string-append "  " stmt ";"))
   (list "}")))

(with-output-to-file "/tmp/er-diagram.dot"
  #:exists 'truncate
  (λ ()
    (for-each displayln lines)))