#lang racket

(require "fad-to-pages.rkt")

;; weirdly, the 2234 fad was double-spaced and missing the first page header.
;; I'm hoping that's the only thing that's wrong with it. This is the code I used
;; to check and fix that. We'll see how 2238 looks.

;; yep... 2238 is broken in the same way, sigh.

;; oog, page header problem at the bottom, too... it's just getting cut 3 lines low everywhere.

;; gee whiz... weird surgery required at the beginning of the summaries... looks like someone
;; is cutting the pages at the wrong points.


;; update... this now fixes the problem for the top, but not the one further down; the
;; current hand-fix is to find the header for the summary page which is actually the
;; page header following the last faculty listing page, and artificially replace it
;; with the page-minus-one header from the following summary page. Finally, I deleted
;; the final three lines (the header of the following page) manually. Basically, all
;; of the cut points are off by 3 lines



(define qtr 2244)


;; given a string ending with a number, return a string ending with the new number instead.
;; the new number string can be no longer than the old one
(define (replace-num str num)
  (match str
    [(regexp #px"^(.*[^0-9])([0-9]+)$" (list _ pre old-num-str))
     (define new-num-str (~r num #:pad-string " " #:min-width (string-length old-num-str)))
     (when (< (string-length old-num-str) (string-length new-num-str))
       (error 'replace-num "expected new number no longer than old number, got ~e and ~e"
              new-num-str old-num-str))
     (string-append pre new-num-str)]))

(module+ test
  (require rackunit)
  (check-equal? (replace-num "abcd 34" 33) "abcd 33")
  (check-equal? (replace-num "abcd 100" 99) "abcd  99"))

(define (go path)
  (define s (file->string (build-path path (~a "fad-" qtr ".txt"))))

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

  (define lines (regexp-split #px"\n" un-doubled))

  ;; this is ridiculous; we're searching for the next page header, and prepending
  ;; it (with the page number artificially decremented) to the beginning of the
  ;; text file. This is to compensate for the fact that somehow, the first three
  ;; lines of the FAD are geting clipped off.
  (define new-prefix
    (let loop ([lines lines]
               [i 1])
      (cond [(empty? lines)
             (error 'ouch "scanned through whole file, didn't find header line")]
            [(and (equal? (first lines) "1")
                  (<= 5 (length lines)))
             (match (list (second lines) (third lines))
               [(list (regexp page-header-2-regexp (list _ mt date mt2 pagenum))
                      (regexp page-assignments-sub-header-regexp))
                (printf "matches on lines ~v through ~v\n"
                        i (+ i 3))
                (unless (and (equal? mt "")
                             (equal? mt2 ""))
                  (error 'scanning
                         "unexpected non-empty values for mt1 and mt2, regexp problem"))
                (define new-num (sub1 (string->number pagenum)))
                (when (< new-num 0)
                  (error 'scanning "new page number would be less than zero: ~e" new-num))
                (list* (first lines)
                       (replace-num (second lines) new-num)
                       (drop (take lines 4) 2))]
               [other
                (error 'scanning "fail on 2")])
             #;(cond [(and (pair? (rest lines))
                           (regexp-match page-header-2-regexp (second lines)))
                      (cond [(and (pair? (rest (rest lines)))
                                  (regexp-match page-assignments-sub-header-regexp
                                                (first (rest (rest lines)))))
                         
                             (list
                              (regexp-match page-header-2-regexp (second lines))
                              (regexp-match page-assignments-sub-header-regexp (third lines))
                              (take lines 3))]
                            [else
                             (error 'scanning "got 2 lines, didn't find third")])]
                     [else ])]
          
            [else
             (loop (rest lines) (add1 i))])))

  (define new-prefix-str
    (apply string-append (add-between new-prefix "\n")))
  
  (display-to-file
   (string-append new-prefix-str "\n" un-doubled)
   (build-path "/tmp/" (~a "fad-" qtr "-amended.txt"))))