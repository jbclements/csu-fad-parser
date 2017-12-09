#lang racket

;; old old old

#;(
(require "fad-parser.rkt"
         ;;"../pages-to-parsed.rkt"
         "../all-data.rkt"
         racket/date
         plot)


;; one-stat: the change of a single statistic across faculty and time
#;(define (one-stat parseds short-dept-name column)
  (define dept-qtrs
    (for/list ([g parseds])
      (map
       un-labwash
       (dept-instructors 
        (list-find short-dept-name (parsed-depts g) #:key 
                   (lambda (x) (short-dept-name (dept-name x))))))))
  (define names
    (apply
     set-union
     (for/list ([d dept-qtrs])
       (list->set
        (map instructor-name d)))))
  (define iq-table
    (for/fold ([ht (hash)]) ([instructors dept-qtrs] [i (in-naturals)])
      (for/fold ([ht ht]) ([instructor instructors])
        (hash-set ht (list (instructor-name instructor) i) 
                  instructor))))
  (for/list ([name (in-set names)])
    (list 
     name
     (for/list ([qtr (length dept-qtrs)])
       (define try-lookup (hash-ref iq-table (list name qtr) #f))
       (cond [try-lookup 
              (string->number/0
               (col-ref 'scu 
                        (instructor-summary try-lookup)))]
             [else 0.0])))))




#;(define (idx->date i)
  (list-ref dates i))

;; unbound var groupeds?
#;(
(define data (one-stat groupeds "CSC" 'total-wtu))

(define (mean-of-second l)
  (/ (apply + (second l)) (length (second l))))

(define sorted-data
  (sort data 
        <
        #:key mean-of-second))

(plot-file
 #:title "distribution of somethings"
 #:y-label "instructor"
 #:x-label "something"
 #:width 800
 #:height 500
 (apply
  mix
  (for/list ([t sorted-data] [i (in-naturals)] [color-idx (in-naturals)])
    (define sorted-vals (sort (second t) <))
    (define q-size (/ (length sorted-vals) 4))
    (define first-quartile (list-ref sorted-vals (round q-size)))
    (define second-quartile (list-ref sorted-vals (round (* 2 q-size))))
    (define third-quartile (list-ref sorted-vals (round (* 3 q-size))))
    (mix
     (rectangles
      #:color color-idx
      (list (vector (ivl first-quartile second-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))
            (vector (ivl second-quartile third-quartile)
                    (ivl (- i 1/4) (+ i 1/4)))))
     (points
      #:color color-idx
      (list (vector (first sorted-vals) i)
            (vector (last sorted-vals) i))))
    #;(points
       ;;#:label (first t)
       #:color color-idx
       (for/list ([pt (second t)])
         (vector pt (add1 i))))))
 "/tmp/faculty-tmp.pdf")


#;(define (stats->file data title y-axis-label filename)
  (parameterize ([plot-x-ticks (date-ticks)])
  (plot-file
   #:title title
   #:y-label y-axis-label
   #:x-label "quarter"
   #:width 800
   #:height 500
   (apply
    mix
    (for/list ([t data] [color-idx (in-naturals)])
      (lines
       #:label (first t)
       #:color color-idx
       (for/list ([pt (second t)] [i (in-naturals)])
         (vector (idx->date i) pt)))))
   filename)))

#;(stats->file
 
 "total wtus by faculty member" "wtu"
 "/tmp/cs-wtus.pdf")))
