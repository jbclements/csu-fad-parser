#lang typed/racket

;; parse all of the fad reports, write them to corresponding
;; names in the same directory.

(require "one-quarter-data.rkt"
         "parsed-data-defn.rkt")

(define (go)
  (parameterize ([pretty-print-columns 500])
    (for ([qtr (in-list (qtrs-available))])
      (call-with-output-file (~a "/tmp/fad-"qtr"-parsed.rktd")
        #:exists 'truncate
        (λ ([port : Output-Port]) (pretty-write (qtr->parsed qtr) port))))))

