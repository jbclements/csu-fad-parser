#lang racket

(require "../parsed-data-defn-untyped.rkt")

(define data
  (for/list ([f (in-list (directory-list "/tmp/"))]
             #:when (regexp-match #px"fad-21[567].-parsed.rktd" f))
    (file->value (build-path "/tmp/" f))))

(define instructors
  (apply
   append
   (map Dept-instructors
        (apply append (map Parsed-depts data)))))

(define headers (map Instructor-header instructors))

(define fieldnum 0)

(= 1
   (length (remove-duplicates (map (λ (header) (map first header)) headers)))

   )

(remove-duplicates (map (λ (header) (map first header)) headers))

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

'((id other-id name rank tsf iaf adm-lvl osf split split-frac iff))
;; frac-pattern: #px"[0-9]*\\.[0-9]+", safe to use string->number
;; iaf : frac-pattern but could also be blank... but there's a clean
;; switch from 0.0 to blank in 2144. So blank is safe to treat as 0.0.
;; osf: frac. no blanks.
;; split appt: 5-digit codes indicate departments. I only have info
;;  for school of engineering (52) departments, so I translate
;;  these to strings, others are left as numbers. I'd love to fix
;;  this, but I don't think I should spend the time, now.
;; split-frac: can be blank. up til 2142, it was otherwise a straight
;;  number. in 2144+, it looks like e.g. "IFF=0.220".
;; iff : always a number. 

(define fieldvals
  (map (λ (header) (second (list-ref header 9))) headers))

(length (remove-duplicates fieldvals))


(define fv2
  (group-by
   first
  (remove-duplicates
   (filter (λ (x) (not (equal? (second x) "")))
           (map (λ (header) (list
                             (second (third header))
                             (second (list-ref header 9)))) headers)))))

fv2



#;(



(length (filter (λ (x) (equal? x "")) fieldvals))
(length (filter (λ (x) (and (string->number x)
                            (= (string->number x) 0)))
                fieldvals))

(define (maybe-deptnum n)
  ;; 52 is school of engineering
  (cond [(<= 52000 n 52999)
         (second (assoc (- n 52000) dept-number-mapping))]
        [else n]))

;; dept-num-parsing:
#;[(regexp #px"SPLIT APPT ([ 0-9]+)" (list _ depts))
   (define nums (map string->number (regexp-split #px" +"
                                                  (string-trim
                                                   depts))))
   (map maybe-deptnum nums)]

(map (λ (g) (list (first g) (length g)))
(group-by
 (λ (x) x)
 (map (λ (header)
        (match (second (list-ref header 10))
          #;[(regexp #px"SPLIT APPT ([ 0-9]+)" (list _ depts))
           (define nums (map string->number (regexp-split #px" +"
                                                          (string-trim
                                                           depts))))
           (map maybe-deptnum nums)]
          [(regexp #px"^[0-9]*\\.[0-9]+$" (list m))
           (cond [(not (= (string->number m) 0)) 'ok]
                 [else 0.0])]
          [other other]))
      headers))))

;(map (λ (summary) (list-ref summary fieldnum)) summaries)