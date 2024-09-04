#lang racket

(require sugar)

(define sources
  (call-with-input-file "/tmp/instructor-data.rktd"
    read))
(define sources-l (range (vector-length sources)))

;; remove the quarter, and then take only uniques
(define raw-tuples
  (map (λ (t) (list->vector (rest (vector->list t))))(vector->list sources)))

(define tuples
  (remove-duplicates raw-tuples))

"number of lines"
(length raw-tuples)

"number of unique tuples"
(length tuples)

"number of pairs"
(expt (length tuples) 2)

;; comparing a pair of these tuples. There are three fields: name, ssnx, and emplid.
;; comparing the name provides either a match (N) or a non-match (NX).
;; comparing the ssnx provides either a match (S) or a non-match (SX).
;; The
;; emplid is the most interesting one, in that it can be blank, which is to say missing.
;; this means that there are three states for the emplid: both are defined and match (E),
;; one or both are undefined (EU), or they are both defined and don't match: (EX)


;; we have twelve bins. The vast majority will be those that have no
;; match: NX,SX,EU or NX,SX,EX. Also, because we removed duplicates, we won't see any
;; N,S,E pairs (these would have been combined).

(define (tup-name t)
  (vector-ref t 0))

(define (tup-ssnx t)
  (vector-ref t 1))

(define (tup-emplid t)
  (vector-ref t 2))

(define (n-check t1 t2)
  (cond [(equal? (tup-name t1) (tup-name t2)) 'N]
        [else 'NX]))

(define (s-check t1 t2)
  (cond [(equal? (tup-ssnx t1) (tup-ssnx t2)) 'S]
        [else 'SX]))

(define (e-check t1 t2)
  (define e1 (tup-emplid t1))
  (define e2 (tup-emplid t2))
  (cond [(equal? e1 "") 'EU]
        [else
         (cond [(equal? e2 "") 'EU]
               [else (cond [(equal? e1 e2) 'E]
                           [else 'EX])])]))

(define grouped-by-name (group-by tup-name tuples))

;; don't use this for big groups...
(define (pairs-from-group g)
  (match g
    ['() '()]
    [(cons f r)
     (append (map (λ (r-elt) (list f r-elt)) r)
             (pairs-from-group r))]))

(require rackunit)
(check-equal? (pairs-from-group '(1 2 3))
              '((1 2) (1 3) (2 3)))

(define z1
(apply append
       (for/list ([g (in-list grouped-by-name)])
         (for/list ([p (in-list (pairs-from-group g))])
           (match-define (list a b) p)
           (list 'N (s-check a b) (e-check a b))))))

(define z2
(apply append
       (for/list ([g (in-list (group-by tup-ssnx tuples))])
         (for/list ([p (in-list (pairs-from-group g))]
                    #:when (equal? (n-check (first p) (second p)) 'NX))
           (match-define (list a b) p)
           (list 'NX 'S (e-check a b))))))

(define z3
  (apply append
         (for/list ([g (in-list
                        (group-by tup-emplid
                                  (filter (λ (t) (not (equal? (tup-emplid t) "")))
                                          tuples)))])
           (for/list ([p (in-list (pairs-from-group g))]
                      #:when (and (equal? (n-check (first p) (second p)) 'NX)
                                  (equal? (s-check (first p) (second p)) 'SX)))
             (list 'NX 'SX 'E)))))

(frequency-hash (append z1 z2 z3))

'#hash(((N SX EU) . 1) ;; possibly: N D ELLIS
       ((NX S EU) . 107) ;; checked, found 4 wins
       ((NX S E) . 11) ;; match
       ((N SX E) . 2) ;; match
       ((NX S EX) . 46) ;; no
       ((N S EU) . 257)) ;; match

;; plan: group all with E, group all with N/S, add pairs from below, then
;; check for bad things: EX, or NX/SX/EU, and NX/S/EU beyond those below

;; all 12:

;; N/S/E : match
;; N/SX/E : match (somewhat scary, but observed with maybe dummy ssnxs)
;; NX/S/E : match
;; NX/SX/E : check by hand (none observed)

;; N/S/EU : match
;; N/SX/EU : check by hand (none observed)
;; NX/S/EU : check by hand (found 4 matches)
;; NX/SX/EU : no match

;; N/S/EX : scary, check by hand
;; N/SX/EX : check by hand (none observed)
;; NX/S/EX : no match
;; NX/SX/EX : no match

;; all of the NX S EU pairs:
(define name-pairs
  (remove-duplicates
   (map (λ (tups)
          (sort (map tup-name tups) string<?))
        (apply append
               (for/list ([g (in-list (group-by tup-ssnx tuples))])
                 (for/list ([p (in-list (pairs-from-group g))]
                            #:when (equal? (n-check (first p) (second p)) 'NX)
                            #:when (equal? (e-check (first p) (second p)) 'EU))
                   p))))))
name-pairs

"same initials:"
(filter (λ (np)
          (equal? (substring (first np) 0 4)
                  (substring (second np) 0 4)))
        name-pairs)

;; win:
(define win-pairs
  '(
    (#("T A MIGLER" "XXXXX1589" "000000000046016")
     #("T A MIGLER-VONDOLLEN" "XXXXX1589" ""))
    (#("J X ZHANG" "XXXXX9825" "000000000128358")
     #("X   ZHANG" "XXXXX9825" ""))
    (#("L L THOMPSON" "XXXXX1461" "000000000036045")
     #("L T SCHLEMER" "XXXXX1461" ""))
    (#("V D CALLOW-ADAMS"
       "XXXXX4784"
       "000000007453650")
     #("V D CALLOW" "XXXXX4784" ""))
    ))

;; the plan: take the e-matches. Then fold together all N/S/EU pairs.
;; Then make the additional two connections shown in win-pairs
