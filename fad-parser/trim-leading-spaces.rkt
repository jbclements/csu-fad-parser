#lang racket

;; WORK IN PROGRESS

(define l
  (file->lines "/Users/clements/clements/datasets/FAD/fad-2174.txt"))

;; given a number n, return #t if there's any information (characters
;; other than space and \f) in the first 'n' columns
;; could be more efficient, but seems to work fine for several thousand
;; lines of input.
(define (info? lines cols)
  ;; the first n chars of each line
  (define prefix-strs
    (remove-duplicates (map (first-n-cols cols) lines)))
  ;; all of the chars in any of the strings
  (define prefix-chars
    (remove-duplicates
     (string->list (apply string-append prefix-strs))))
  ((compose not empty?)
   (remove*
    '(#\space #\page)
    prefix-chars)))

(define (first-n-cols cols)
  (λ (str)
    (substring str 0
               (min cols (string-length str)))))

(define maxstrlen (apply max (map string-length l)))
(define (first-interesting-column lines)
  (sub1
   (for/first ([cols (in-range maxstrlen)]
               #:when (info? lines cols))
     cols)))

(require rackunit)
(check-equal? (first-interesting-column '("abc")) 0)
(check-equal?
 (first-interesting-column '("    abcd" "   def" "     ghi")) 3)


(define cols-to-discard (first-interesting-column l))



