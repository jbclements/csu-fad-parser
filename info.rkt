#lang info

(define collection 'multi)
(define deps '("base"
               "db-lib"
               "explorer"
               "memoize"
               "parser-tools-lib"
               "plot-gui-lib"
               "plot-lib"
               "typed-racket-lib"
               "csse-scheduling"
               "csv-writing"
               "threading"))
(define build-deps '("rackunit-lib"
                     "wxme-lib"
                     "typed-racket-more"))
