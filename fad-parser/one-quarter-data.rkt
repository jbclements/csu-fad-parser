#lang typed/racket

;; this file accepts a quarter number and returns a Parsed.
;; it knows where the files are stored, and which format
;; to use for which quarter.

(require "parsed-data-defn.rkt"
         "pages-to-parsed-tr.rkt"
         "some-structs.rkt"
         threading
         (only-in "divide-columns.rkt" Format))

(require/typed "fad-to-pages.rkt"
               [file->fad-pages (Path-String Format -> fad-pages)])

;; given a quarter number such as 2178, return a Parsed
;; representing the FAD data
(: qtr->parsed (Natural -> Parsed))

;; return a list of all quarters for which we have FAD reports
(: qtrs-available (-> (Listof Natural)))

(provide qtr->parsed
         qtrs-available)

(define FAD-DIRECTORY (build-path "/Users/clements/OneDrive - Cal Poly/datasets/FAD"))

;; return a list of all the quarters for which we have
;; FAD reports
(define (qtrs-available) : (Listof Natural)
  (remove-duplicates
   (for/list : (Listof Natural)
     ([f (in-directory FAD-DIRECTORY)]
      #:when (and (file-exists? f)
                  (regexp-match fad-filename-pattern
                                (filename-only f))))
     (match (filename-only f)
       [(regexp fad-filename-pattern (list _ qtr-str amended?))
        ;; pattern guarantees this assert should succeed
        (assert (string->number (assert qtr-str string?))
                exact-nonnegative-integer?)]))))

;; return the final filename portion of a filename, ensuring it's a string
(define (filename-only [f : Path-String]) : String
  (define-values (stem final must-be-dir?) (split-path f))
  (cond [must-be-dir? (raise-argument-error 'filename-only
                                            "path that can be a filename"
                                            0 f)]
        [else (cond
                [(or (equal? final 'up) (equal? final 'same))
                 (raise-argument-error 'filename-only
                                       "path not ending with . or .."
                                       0 f)]
                [else (path->string final)])]))

;; the pattern that fad filenames have:
(define fad-filename-pattern #px"^fad-([0-9]+)(-amended)?\\.txt$")

;; let's find bugs in this file:
#;(define d
  (file->parsed (build-path FAD-DIRECTORY "fad-2174.txt")
              '2174-fmt))

;; what's the format of this quarter?
(define (qtr->fformat [cpqtr : Natural]) : Format
  (cond [(< cpqtr 2144) 'pre-2144]
        [(< cpqtr 2168) 'post-2142]
        [(< cpqtr 2174) 'post-2164]
        [else '2174-fmt]))

(define (qtr->filename [qtr : Natural]) : Path
  (define p1 (build-path FAD-DIRECTORY (~a "fad-"qtr"-amended.txt")))
  (define regular-path (build-path FAD-DIRECTORY (~a "fad-"qtr".txt")))
  (cond [(file-exists? p1) p1]
        [else regular-path]))


;; given a filename and a format, return a Parsed
(: file->parsed (Path-String Format -> Parsed))
(define (file->parsed file fformat)
  (~> file
      (file->fad-pages fformat)
      (parse-pages fformat)))

(define (qtr->parsed [qtr : Natural]) : Parsed
  (file->parsed (qtr->filename qtr) (qtr->fformat qtr)))

