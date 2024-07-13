#lang typed/racket

(provide (struct-out fad-pages)
         (struct-out dept-pages)
         (struct-out page)
         (struct-out assignments-page)
         (struct-out summary-page))

(define-type Summary-Label
  (U "FULL TIME" "PART TIME" "SUBTOTAL" "OTHER" "TOTAL" "SALARY RANGE TITLE"
     "PROFESSOR/LECT D" "ASSOC PROF/LECT C" "ASST PROF/LECT B" "INSTRUCTOR/LECT A"
     "TCHNG ASSOCIATE"
     "TCHG ASSOCIATE"
     "SUBTOTAL"
     "ADMINISTRATOR" "ADMINSTRATOR"
     "OTHER" "SUBTOTAL" "TOTAL"

     "TCHG ASST/LECT L"

     "GRAD ASSISTANT"
     ;; whoa, giant hacks here...
     "SALARY" "RANGE TITLE" "-"))

(define-type Summary (Listof (List Summary-Label (Listof String))))

;; a fad-pages contains an assoc-list and (listof dept-pages)
;; college-summary-ex: '("FULL TIME" ("" "167" "152.28" "1486.1"
;; "249.4" "1735.5" "0.0" "1735.5" "11.40" "11.40" "36839.6" "2467.3" "241.92" "16.20"))
(struct fad-pages ([college-summary : (U Summary 'no-college-summary-page)]
                   [depts : (Listof dept-pages)]) #:transparent)

;; a dept-pages is (dept-pages string assoc-list (listof page))
;; a dept-pages contains a string, a summary, and a list of line-tokens 
(struct dept-pages ([name : String] [summary : Summary]
                                    [detail : (Listof Any)]) #:transparent)


;; a page has a date, two numbers, some labels, a department, and some lines.
(define-struct page ([date : Any] [page-a : String] [page-b : String]
                                  [labels : (Listof Any)]
                                  [dept : (List (U False String))]
                                  [lines : (Listof Any)]) #:transparent)

;; depending on the header, they're either ASSIGNMENTS ...
(define-struct (assignments-page page) ())
;; ... or SUMMARY pages.
(define-struct (summary-page page) ())
