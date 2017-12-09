#lang racket

(filter
 (λ (x) (not (second x)))
(for/list ([f (in-list (directory-list "/tmp/"))]
           #:when (regexp-match #px"fad-2...-parsed.rktd" f))
  (list
   f
   (equal? (file->value (build-path "/tmp/" f))
          (file->value (build-path "/Users/clements/clements/datasets/FAD/parsed/" f))))))