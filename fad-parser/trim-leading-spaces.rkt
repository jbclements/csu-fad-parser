#lang racket

;; used to left-justify a FAD report by trimming all-blank columns.

(provide first-interesting-column/file)


;; is this character interesting?
(define (interesting? ch)
  (not (or (eq? ch #\space)
           (eq? ch #\page))))

;; return the index of the first interesting column, or #f
;; if there are no interesting columns
(define (first-interesting-column str)
  (for/first ([i (in-range (string-length str))]
              #:when (interesting? (string-ref str i)))
    i))


;; return the smallest "first interesting column" from the
;; given lines
(define (first-interesting-column/lines lines)
  (define col-idxes
    (filter (λ (x) x)
            (map first-interesting-column
                 lines)))
  (cond [(empty? col-idxes)
         (raise-argument-error
          'first-interesting-column
          "lines containing at least one interesting character"
          0 lines)]
        [else
         (apply min col-idxes)]))

;; find the first interesting column in any line in the file.
(define (first-interesting-column/file f)
  (first-interesting-column/lines (file->lines f)))

(module+ test

  (require rackunit)

  (check-equal? (first-interesting-column "abc") 0)
  (check-equal? (first-interesting-column " \fabc") 2)
  (check-equal? (first-interesting-column " \f  ") #f)

  (check-equal? (first-interesting-column/lines '("abc")) 0)
  (check-equal?
   (first-interesting-column/lines
    '("    abcd" "   def" "     ghi")) 3))



;; exploring...
#;(for/list ([f (in-directory "/Users/clements/clements/datasets/FAD/")]
           #:when (regexp-match? #px"fad-[[:digit:]]+.txt$" f))
  (define col (first-interesting-column/file f))
  (cond [(<= 2 col) 'okay]
        [else
         (define shortest-lines
           (filter (λ (l)
                     (define lcol (first-interesting-column l))
                     (and (number? lcol) (= lcol col)))
                   (file->lines f)))
         (list f
               col
               (take
                shortest-lines
                (min 3 (length shortest-lines))))]))

