#lang racket

(require db/base
         db/postgresql)

(define conn
  (postgresql-connect #:user "fad_owner"
                      #:database "fad"
                      #:port 5432))

(query-exec
 conn
 "CREATE TEMP TABLE \"classifications\" (
  \"id\" integer NOT NULL,
  PRIMARY KEY (\"id\")
);")

(query-exec
 conn
 "CREATE TEMP TABLE \"instructors\" (
  \"id\" varchar(128) NOT NULL,
  \"name\" varchar(128) NOT NULL,
  \"soc\" varchar(9) NOT NULL,
  PRIMARY KEY (\"id\")
);"
)

(define (add-instructor! id name soc)
  (match (query-rows
          conn
          "SELECT * FROM instructors WHERE id=$1"
          id)
    [(list (vector _ name2 soc2))
     (unless (equal? (list name2 soc2)
                     (list name soc))
       (raise-argument-error 'add-instructor!
                             "record that doesn't conflict with existing"
                             0 (list id name soc)))]
    [(list)
     (query-exec
      conn
      "INSERT INTO instructors VALUES ($1,$2,$3)"
      id name soc)]))

(module+ test
  (require rackunit)
  (check-not-exn
   (λ ()
     (add-instructor! "clements" "John Clements" "XXXX")))
  (check-not-exn
   (λ ()
     (add-instructor! "clements" "John Clements" "XXXX")))
  (check-exn
   #px"record that doesn't conflict"
   (λ ()
     (add-instructor! "clements" "John Clements" "XXaXX")))
  (check-equal? 
   (query-rows
    conn
    "SELECT * FROM instructors;")
   '(#("clements" "John Clements" "XXXX"))))

