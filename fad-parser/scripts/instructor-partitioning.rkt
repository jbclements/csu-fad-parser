#lang racket

(require data/union-find)


(define sources
  (call-with-input-file "instructor-data.rktd"
    read))
(define sources-l (range (vector-length sources)))

;; given an index into sources, return the corresponding emplid
(define (emplid idx)
  (vector-ref (vector-ref sources idx) 3))

(define (name idx)
  (vector-ref (vector-ref sources idx) 1))

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

(define h2 (hash-group-by sources-l name-and-ssn))

;; use uf-union to join each group into one
(for ([(_ sames) (in-hash h2)])
  (define a (first sames))
  (for ([b (rest sames)])
    (uf-union! (vector-ref sets a) (vector-ref sets b))))



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

;; extract the groups from the union-find sets:

(define idx-groups
  (hash-group-by sources-l
                 (λ (idx) (uf-find (vector-ref sets idx)))))

(define groups
  (for/list ([(_ idxes) (in-hash idx-groups)])
    (map (λ (idx) (vector-ref sources idx)) idxes)))

(length groups)

;; check that no group contains emplids that conflict:

(for ([g groups])
  (define deduplicated-emplids
    (remove-duplicates
     (filter (λ (s) (not (equal? s "")))
             (map (λ (r) (vector-ref r 3)) g))))
  (when (< 1 (length deduplicated-emplids))
    (error 'emplid-check "group contains more than one non-empty emplid: ~e"
           g)))



#;(for ([i (in-range (vector-length sources))])
  (for ([j (in-range (add1 i) (vector-length sources))])
    (cond [(and (not (equal? (emplid i) ""))
                (not (equal? (emplid j) ""))
                (equal? (emplid i) (emplid j)))
           (uf-union! (vector-ref sets i) (vector-ref sets j))]
          [else #f])))



