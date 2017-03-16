#lang typed/racket

;; this file accepts a quarter number and returns a Parsed.
;; it knows where the files are stored, and which format
;; to use for which quarter.

(require "pages-to-parsed-tr.rkt"
         (only-in "divide-columns.rkt" Format))

(provide qtr->parsed)

(define FAD-DIRECTORY (build-path "/Users/clements/clements/datasets/FAD"))

;; let's find bugs in this file:
#;(define d
  (file->parsed (build-path FAD-DIRECTORY "fad-2172.txt")
              'post-2164))

;; what's the format of this quarter?
(define (qtr->fformat [cpqtr : Natural]) : Format
  (cond [(< cpqtr 2144) 'pre-2144]
        [(< cpqtr 2168) 'post-2142]
        [else 'post-2164]))

(define (qtr->filename [qtr : Natural]) : Path
  (build-path FAD-DIRECTORY (~a "fad-"qtr".txt")))

(define (qtr->parsed [qtr : Natural]) : Parsed
  (file->parsed (qtr->filename qtr) (qtr->fformat qtr)))

