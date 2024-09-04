#lang racket

(require data/union-find
         tabular-asa)


(define sources
  (call-with-input-file "/tmp/instructor-data.rktd"
    read))
(define sources-l (range (vector-length sources)))

;; given an index into sources, return the corresponding emplid
(define (emplid idx)
  (vector-ref (vector-ref sources idx) 3))

(define (name idx)
  (vector-ref (vector-ref sources idx) 1))

(define (ssnx idx)
  (vector-ref (vector-ref sources idx) 2))

(define (name-and-ssn idx)
  (define elt (vector-ref sources idx))
  (cons (vector-ref elt 1) (vector-ref elt 2)))

(define sets
  (vector-map uf-new sources))

(define (hash-group-by l key)
  (define h (make-hash))
  (for ([elt l])
    (define k (key elt))
    (when k
      (hash-set! h k (cons elt (hash-ref h k '())))))
  h)

;; extract the groups from the union-find sets:
(define (get-groups)
  (define idx-groups
    (hash-group-by sources-l
                   (λ (idx) (uf-find (vector-ref sets idx)))))

  (for/list ([(_ idxes) (in-hash idx-groups)])
    (map (λ (idx) (vector-ref sources idx)) idxes)))

"before starting"
(length (get-groups))

;; collapse those with the same emplid:

(define h1 (hash-group-by sources-l
                          (λ (idx)
                            (define e (emplid idx))
                            (cond [(equal? e "") #f]
                                  [else e]))))

;; use uf-union to join each group into one
(for ([(_ sames) (in-hash h1)])
  (define a (first sames))
  (for ([b (rest sames)])
    (uf-union! (vector-ref sets a) (vector-ref sets b))))

"post emplid join"
(length (get-groups))

;; now join those with the same name and ssnx:

(define h2 (hash-group-by sources-l name-and-ssn))

;; use uf-union to join each group into one
(for ([(_ sames) (in-hash h2)])
  (define a (first sames))
  (for ([b (rest sames)])
    (uf-union! (vector-ref sets a) (vector-ref sets b))))

"post name/ssnx join"
(length (get-groups))

(define representatives
  (remove-duplicates
   (for/list ([i (in-range (vector-length sources))])
     (uf-find (vector-ref sets i)))))

;; find rows with matching names that are not currently paired:

(define h3 (hash-group-by sources-l name))

"sets that share a name:"
(apply
 append
 (for/list ([(k idxes) h3])
   (define set-representatives
     (remove-duplicates
      (map (λ (idx) (uf-find (vector-ref sets idx))) idxes)))
   (cond [(< 1 (length set-representatives))
          (list (list k set-representatives))]
         [else
          '()])))



(length (get-groups))

(call-with-output-file "/tmp/groups.rktd"
  #:exists 'truncate
  (λ (port)
    (pretty-write (get-groups) port)))

;; join some explicitly:
(define to-join
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

    (#("N D ELLIS" "XXXXX0002" "")
     #("N D ELLIS"
       "XXXXX4006"
       "000000009252174"))
    ))

;; given a vector containing name, ssnx, and emplid, return the corresponding
;; index.
(define (find-idx vec)
  (match-define (vector t-name t-ssnx t-emplid) vec)
  (let loop ([i 0])
    (cond [(<= (vector-length sources) i)
           (error 'find-idx "person not found: ~v\n")]
          [else
           (cond [(and (equal? (name i) t-name)
                       (equal? (ssnx i) t-ssnx)
                       (equal? (emplid i) t-emplid))
                  i]
                 [else (loop (add1 i))])])))

(define (merge-pair tup-1 tup-2)
  (define i (find-idx tup-1))
  (define j (find-idx tup-2))
  (uf-union! (vector-ref sets i) (vector-ref sets j)))

(for ([pr (in-list to-join)])
  (apply merge-pair pr))

(length (get-groups))

;; check that no group contains emplids that conflict:

(define (tup-name t)
  (vector-ref t 1))

(define (tup-ssnx t)
  (vector-ref t 2))

(define (tup-emplid t)
  (vector-ref t 3))

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

;; don't use this for big groups...
(define (pairs-from-group g)
  (match g
    ['() '()]
    [(cons f r)
     (append (map (λ (r-elt) (list f r-elt)) r)
             (pairs-from-group r))]))

(define final-groups (get-groups))

(for ([g final-groups])

  (for ([pr (in-list (pairs-from-group g))])
    (match-define (list t1 t2) pr)
    (define category (list (n-check t1 t2) (s-check t1 t2) (e-check t1 t2)))
    (when (or (equal? (e-check t1 t2) 'EX)
              (member category '((NX SX EU))))
      (eprintf "warning pair in group: ~e and ~e\n"
               t1 t2)
      )))

;; find earliest name for each group:
(define final-table
  (table-read/sequence
   (apply
    append
    (for/list ([g (in-list final-groups)])
      (define earliest-name
        (vector-ref (argmin (λ (v) (vector-ref v 0)) g) 1))
      (map (λ (tup) (cons earliest-name (vector->list tup))) g)))
   '(id qtr name ssnx emplid)))




#;(for ([i (in-range (vector-length sources))])
  (for ([j (in-range (add1 i) (vector-length sources))])
    (cond [(and (not (equal? (emplid i) ""))
                (not (equal? (emplid j) ""))
                (equal? (emplid i) (emplid j)))
           (uf-union! (vector-ref sets i) (vector-ref sets j))]
          [else #f])))



