#lang typed/racket

;; export lists of values as TSV files to be imported by postgres
(provide tsv-export)

(require/typed csv-writing
               [#:opaque CsvPrintingParams csv-printing-params?]
               [make-csv-printing-params
                (#:column-separator String #:table-cell->string (Any -> String) -> CsvPrintingParams)]
               [display-table ((Listof (Listof DbVal)) Output-Port #:printing-params CsvPrintingParams -> Void)])

(require/typed db
               [#:opaque SqlNull sql-null?])

(define-type DbVal (U Boolean 'null String Natural SqlNull))

;; given a filename and a list of records (lists),
;; output the records in tab-separated format to the given filename
(define (tsv-export [filename : Path-String]
                    [records : (Listof (Listof DbVal))])
  (call-with-output-file filename
    (lambda (port)
      (display-table records port #:printing-params postgres-printing-params))
    #:exists 'truncate))



;; given a value, produce the sanitized db-friendly version
(define (postgres-convert-value [d : Any]) : String
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
        [(exact-integer? d) (number->string d)]
        [else (error 'postgres-convert-value
                     "unexpected postgres value: ~e"
                     d)]))

(define postgres-printing-params : CsvPrintingParams
  (make-csv-printing-params
   #:column-separator "\t"
   #:table-cell->string postgres-convert-value))

(module+ test
  (require typed/rackunit)

  (check-equal? (postgres-convert-value 34) "34")
  (check-equal? (postgres-convert-value #f) "FALSE")
  (check-equal? (postgres-convert-value 'null) "\\N")
  #;(check-equal? (sanitize-row '(34 #f null "BOOBOAH"))
                '(34 "\\N" "\\N" "BOOBOAH")
   )
  )
