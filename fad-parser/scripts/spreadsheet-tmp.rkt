#lang racket

(require "../parsed-data-defn-untyped.rkt")

(define data
  (for/list ([f (in-list (directory-list "/tmp/"))]
             #:when (regexp-match #px"fad-....-parsed.rktd" f))
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

;; frac-pattern: #px"[0-9]*\\.[0-9]+", safe to use string->number

(remove-duplicates
 (map (λ (header)`
        (match (second (fifth header))
          [(regexp #px"[0-9]*\\.[0-9]+" (list _)) 'ok]
          [other other]))
      headers))

;(map (λ (summary) (list-ref summary fieldnum)) summaries)