#lang racket

(require "../parsed-data-defn-untyped.rkt")

(define data
  (for/list ([f (in-list (directory-list "/tmp/"))]
             #:when (regexp-match #px"fad-21[6].-parsed.rktd" f))
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



(define fieldvals
  (map (λ (header) (second (ninth header))) headers))

(length (remove-duplicates fieldvals))
(length (filter (λ (x) (equal? x "")) fieldvals))
(length (filter (λ (x) (and (string->number x)
                            (= (string->number x) 0)))
                fieldvals))

(define (maybe-deptnum n)
  ;; 52 is school of engineering
  (cond [(<= 52000 n 52999)
         (second (assoc (- n 52000) dept-number-mapping))]
        [else n]))

(map (λ (g) (list (first g) (length g)))
(group-by
 (λ (x) x)
 (map (λ (header)
        (match (second (ninth header))
          [(regexp #px"SPLIT APPT ([ 0-9]+)" (list _ depts))
           (define nums (map string->number (regexp-split #px" +"
                                                          (string-trim
                                                           depts))))
           (map maybe-deptnum nums)]
          [(regexp #px"^[0-9]*\\.[0-9]+$" (list m))
           (cond [(not (= (string->number m) 0)) 'ok]
                 [else 0.0])]
          [other other]))
      headers)))

;(map (λ (summary) (list-ref summary fieldnum)) summaries)