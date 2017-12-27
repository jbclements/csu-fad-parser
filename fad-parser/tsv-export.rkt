#lang typed/racket

;; export lists of values as TSV files to be imported by postgres
(provide tsv-export)

(require/typed db
               [#:opaque SqlNull sql-null?])

(define-type DbVal (U Boolean 'null String Natural SqlNull))

(define-type Serializable (U String Natural))

;; given a filename and a list of records (lists),
;; output the records in tab-separated format to the given filename
(define (tsv-export [filename : Path-String]
                    [records : (Listof (Listof DbVal))])
  (with-output-to-file filename
    (lambda ()
      (for ([c (in-list records)])
        (define data (sanitize-row c))
        (display (apply ~a (append (add-between data "\t") (list "\n"))))))
    #:exists 'truncate))

;; given a list of values, sanitize them to be db-friendly
(define (sanitize-row [row : (Listof DbVal)]) : (Listof Serializable)
  (map sanitize-value row))

;; given a value, produce the sanitized db-friendly version
(define (sanitize-value [d : DbVal]) : Serializable
  (cond [(sql-null? d) "\\N"]
        [(equal? d 'null)
         "\\N"]
        [(equal? d #t)
         "TRUE"]
        [(equal? d #f)
         "FALSE"]
        [(string? d)
         (when (ormap (λ(ch)
                        (member ch '(#\tab #\newline #\\)))
                      (string->list d))
           (error 'export-data
                  "no bad chars allowed in strings: ~e\n"
                  d))
         d]
        [(exact-integer? d) d]))

(module+ test
  (require typed/rackunit)

  (check-equal? (sanitize-row '(34 #f null "BOOBOAH"))
                '(34 "\\N" "\\N" "BOOBOAH")
   )
  )
