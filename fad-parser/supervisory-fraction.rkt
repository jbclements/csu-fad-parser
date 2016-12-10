#lang racket

(require db
         plot
         racket/date)

(define conn
   (virtual-connection
    (lambda ()
      (mysql-connect #:user "clements"
                 #:database "fad"
                 #:password "aoeuidht"
                 #:port 11306))))

(define depts
  (query-list
   conn
   "SELECT homedept FROM instructorstatuses GROUP BY homedept"))

(define qtrs
  (query-list
   conn
   "SELECT qtr FROM instructorstatuses GROUP BY qtr"))

(query-rows
            conn
            (~a
             "SELECT ist.id, os.subject, os.num, ofs.enrollment FROM (offerseqs AS os), "
             "(instructorstatuses as ist), (offerings AS ofs) "
             "WHERE os.qtr = ist.qtr "
             "AND os.instructor = ist.id "
             "AND ist.homedept = ? "
             "AND os.subject = ofs.subject "
             "AND os.num = ofs.num "
             "AND os.section = ofs.section "
             "AND os.qtr = ofs.qtr "
             "AND os.qtr = 2124 "
             "AND (ofs.classificationcorrected = 25 "
             "OR ofs.classificationcorrected = 36) ")
            "BMGE"
            )
;; supervisory wtus:
(define sups
  (for/list ([d depts])
    (cons d
          (map 
           vector->list
           (query-rows
            conn
            (~a
             "SELECT os.qtr,SUM(os.dwtucorrected) FROM (offerseqs AS os), "
             "(instructorstatuses as ist), (offerings AS ofs) "
             "WHERE os.qtr = ist.qtr "
             "AND os.instructor = ist.id "
             "AND ist.homedept = ? "
             "AND os.subject = ofs.subject "
             "AND os.num = ofs.num "
             "AND os.section = ofs.section "
             "AND os.qtr = ofs.qtr "
             "AND (ofs.classificationcorrected = 25 "
             "OR ofs.classificationcorrected = 36) "
             "GROUP BY qtr ")
            d
            )))))

;; all course-based dwtus:
(define classes
  (for/list ([d depts])
    (cons d
          (map
           vector->list
           (query-rows
            conn
            (~a
             "SELECT os.qtr,SUM(os.dwtucorrected) FROM (offerseqs AS os), "
             "(instructorstatuses as ist), (offerings AS ofs) "
             "WHERE os.qtr = ist.qtr "
             "AND os.instructor = ist.id "
             "AND ist.homedept = ? "
             "AND os.subject = ofs.subject "
             "AND os.num = ofs.num "
             "AND os.section = ofs.section "
             "AND os.qtr = ofs.qtr "
             "GROUP BY qtr ")
            d
            )))))

;; all specialcredit wtus
(define specials
  (for/list ([d depts])
    (cons d
          (map
           vector->list
           (query-rows
            conn
            (~a
             "SELECT sc.qtr,SUM(sc.dwtu + sc.iwtu) "
             "FROM (specialcredits AS sc), "
             "(instructorstatuses as ist)"
             "WHERE sc.qtr = ist.qtr "
             "AND sc.instructor = ist.id "
             "AND ist.homedept = ? "
             "GROUP BY qtr ")
            d
            )))))

(for/list ([d depts])
  (display (~a d"\t"))
  (display 
   (~r #:precision '(= 2)
       (* 100
          (/ (for/sum ((q qtrs))
            (first (dict-ref (dict-ref sups d) q 
                             (list 0))))
          (+ (for/sum ((q qtrs))
               (first (dict-ref (dict-ref classes d) q
                                (list 0))))
             (for/sum ((q qtrs))
               (first (dict-ref (dict-ref specials d) q 
                                (list 0)))))))))
  (newline))



(define (qtr->date q)
  (find-seconds 0 0 0 1 (qtr->month q) (qtr->year q)))

(define (qtr->month q)
  (match (modulo q 10)
    [2 3]
    [4 6]
    [6 9]
    [8 12]))

(define (qtr->year q)
  (+ 2000 (modulo (floor (/ q 10)) 100)))

(parameterize ([plot-x-ticks      (date-ticks)])
(plot-file
 (for/list ([d depts] [i (in-naturals)])
   (lines
    #:label d
    #:color i
    (for/list ([q qtrs][i (in-naturals)])
      (vector
       (qtr->date q)
       (/ (first (dict-ref (dict-ref sups d) q 
                           (list 0)))
          (+ (first (dict-ref (dict-ref classes d) q
                              (list 0)))
             (first (dict-ref (dict-ref specials d) q 
                              (list 0))))))
      )))
 "/tmp/supervisory-frac-over-time.pdf"))



(with-output-to-file "/tmp/supervisory-fraction.txt"
  (lambda ()
    (display "\t")
    (display
     (apply ~a (add-between qtrs "\t")))
    
    (newline)
    (for ([d depts])
      (display (~a d"\t"))
      (display
       (apply
        ~a
        (add-between
         (for/list ([q qtrs])
           (exact->inexact
            (/ (round
                (* 10000
                   (/ (first (dict-ref (dict-ref sups d) q 
                                       (list 0)))
                      (+ (first (dict-ref (dict-ref classes d) q
                                          (list 0)))
                         (first (dict-ref (dict-ref specials d) q 
                                          (list 0)))))))
               100))
           )
         "\t")))
      (newline))))




(disconnect conn)

