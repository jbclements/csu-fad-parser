#lang racket

(require db
         (only-in "pages-to-parsed.rkt" group-by))

(define conn
   (mysql-connect #:user "clements"
                 #:database "fad"
                 #:password "aoeuidht"))

(define tt-faculty (file->value "final-tt.rktd"))

#<<|
SELECT 


|


(disconnect conn)