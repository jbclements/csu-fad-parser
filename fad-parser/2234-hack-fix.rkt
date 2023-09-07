#lang racket

;; weirdly, the 2234 fad was double-spaced and missing the first page header.
;; I'm hoping that's the only thing that's wrong with it. This is the code I used
;; to check and fix that. We'll see how 2238 looks.

;; gee whiz... weird surgery required at the beginning of the summaries... looks like someone
;; is cutting the pages at the wrong points.

(define (go)
  (define s (file->string "/tmp/fad-2234.txt"))

  (define starts-with-lonely-newline?
    (regexp-match #px"^\n[^\n]" s))

  (unless starts-with-lonely-newline?
    (error 'ouch "expected first char of string to be a lonely newline"))

  (define after-first-char (substring s 1 (string-length s)))

  (define has-single-newline?
    (regexp-match #px"[^\n]\n[^\n]" after-first-char))

  (when has-single-newline?
    (error 'ouch "expected all newlines after the first to occur in pairs"))

  (define un-doubled (regexp-replace* #px"\n\n" after-first-char "\n"))

  (define prefix
    "1
 JUNE 5, 2023                        CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES                    PAGE                 200
 JOB APD55   PGM APD60           FACULTY ASSIGNMENTS BY DEPARTMENT FOR SPRING 2023 SAN LUIS OBISPO   

")

  (display-to-file
   (string-append prefix un-doubled)
   "/tmp/fad-2234-amended.txt"))