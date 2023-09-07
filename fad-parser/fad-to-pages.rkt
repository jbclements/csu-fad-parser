#lang racket

;; this file parses a file into a fad-pages. It's the first
;; stage in parsing, and basically splits a file into a
;; categorized list of pages, each containing a bunch
;; of lines.

;; I would love to rewrite this into Typed Racket, but I strongly
;; suspect that trying to use the parser-tools with TR would be
;; a big big pain.

(require parser-tools/lex
         parser-tools/yacc
         racket/generator
         "trim-leading-spaces.rkt"
         (only-in "divide-columns.rkt" format?))

(provide (struct-out fad-pages)
         (struct-out dept-pages)
         (struct-out page)
         (struct-out assignments-page)
         (struct-out summary-page)
         (contract-out
          [file->fad-pages (-> path-string?
                               format?
                               fad-pages?)]
          [summary-column-ref
           (-> symbol? summary-row? number?)])
         page-level
         eof-group
         token-EOF
         summary-row?
         file->tokens)


;; a fad-pages contains an assoc-list and (listof dept-pages)
(struct fad-pages (college-summary depts) #:prefab)

;; a dept-pages is (dept-pages string assoc-list (listof page))
;; a dept-pages contains a string, a summary, and a list of line-tokens 
(struct dept-pages (name summary detail) #:prefab)

;; a page has a date, two numbers, some labels, a department, and some lines.
(define-struct page (date page-a page-b labels dept lines) #:prefab)
;; depending on the header, they're either ASSIGNMENTS ...
(define-struct (assignments-page page) ())
;; ... or SUMMARY pages.
(define-struct (summary-page page) ())

(define summary-row? (list/c string? (listof string?)))


;; file->fad-pages : path-string format-symbol -> fad-pages
(define (file->fad-pages filename format)
  (display (~a "parsing file: "filename"\n"))
  (define the-pages (file->pages filename format))
  (printf "read ~v file pages\n" (length the-pages))
  (define result (pages->fad-pages the-pages))
  (display (~a "found "
               (length (fad-pages-depts result))
               " departments.\n"))
  result)


;;;;;;;;
;;
;;  PARSING FILES INTO PAGES
;;
;;;;;;;;


;; accepts a filename and a symbol indicating which family of fads this comes from
(define (file->lines-thunk filename format)
  (define left-margin-chars (first-interesting-column/file filename))
  (printf "first interesting column: ~v\n"
          left-margin-chars)
  ;; it would probably be cleaner to apply this to all files, but
  ;; it's easier just to apply it to the new ones:
  (define (trim-left-margin str-or-eof)
    (match format
      ['2174-fmt
       (cond [(not (string? str-or-eof)) str-or-eof]
             [(< (string-length str-or-eof) left-margin-chars) ""]
             [else (substring str-or-eof left-margin-chars)])]
      [other
       str-or-eof]))
  (define fileport (open-input-file filename))
  (port-count-lines! fileport)
  (define lines-generator
    (sequence->generator
     (in-port (lambda (p) 
                (list (lexer-pos p)
                      (trim-left-margin (read-line p))
                      (lexer-pos p)))
              fileport)))
  (define regexp-pairings (fmt->regexp-pairings format))
  (define (get-token)
    (match-define (list pre line post) (lines-generator))
    ;; eliminate blank lines ahead of time:
    (cond [(and (string? line) (regexp-match #px"^[ \r]*$" line)) (get-token)]
          [(eof-object? line) (position-token (token-EOF) #f #f)]
          [else (position-token ((page-line-tokenize regexp-pairings) line)
                                pre post)]))
  get-token)

;; render the port's position as a lexer position
(define (lexer-pos port)
  (define-values (line column offset) (port-next-location port))
  (position offset line column))


(define-tokens page-level (STRAY-NUM-LINE
                           PAGE-HEADER-LINE
                           PAGE-HEADER-MINIMAL-LINE
                           PAGE-ASSIGNMENTS-LINE
                           PAGE-SUMMARY-LINE
                           PAGE-SUMMARY-2-LINE
                           DEPARTMENT-LINE
                           NO-DEPARTMENT-LINE
                           ASSIGN-HDR1
                           ASSIGN-HDR2
                           ASSIGN-HDR3
                           SUMMARY-HDR1
                           SUMMARY-HDR2
                           INSTRUCTOR-HDR
                           INSTRUCTOR-TOTS-DIV
                           INSTRUCTOR-DIV
                           INSTRUCTOR-COURSE-HDR
                           INSTRUCTOR-SPLIT-APPOINTMENT-LINE
                           INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1
                           INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2
                           ;; body lines
                           OTHER-LINE))
(define-empty-tokens eof-group (EOF))

(define page-header-regexp
  #px"^\\s*(1DATE|)\\s*(\\d\\d/\\d\\d/\\d\\d)\\s+CHANCELLOR'S OFFICE\\s+THE CALIFORNIA STATE UNIVERSITIES\\s+PAGE (\\d+)-\\s*(\\d+)")

;; bogus ()'s to match format of old regexp, allowing reuse of token
(define page-header-2-regexp
  #px"^()\\s*([A-Z]+ \\d+, [0-9]{4})\\s+CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES\\s+PAGE\\s*()(\\d+)")

(define page-header-minimal-regexp
  #px"^\\s*CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES\\s*$")

(define page-header-minimal-regexp-post-2164
  #px"^\\s*[A-Z]+\\s+[0-9]+,\\s+[0-9]+\\s*CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES\\s*$")

(define page-assignments-sub-header-regexp
  ;; always 5-char JOB and PGM?
  #px"^\\s+JOB APD(\\d\\d)\\s+PGM APD(\\d\\d)\\s+FACULTY ASSIGNMENTS BY DEPARTMENT FOR (.*)")

(define page-summary-sub-header-regexp
  ;; always 5-char JOB and PGM?
  #px"^\\s+JOB APD(\\d\\d)\\s+PGM APD(\\d\\d)\\s+SUMMARY BY FACULTY TYPE AND RANK FOR (.*)")

(define page-summary-sub-header-2-regexp
  ;; always 5-char JOB and PGM?
  #px"^\\s+JOB APD(\\d\\d)\\s+PGM APD(\\d\\d)\\s+SUMMARY BY FACULTY TYPE AND RANK FOR ([A-Z]+\\s+\\d+\\s+SAN LUIS OBISPO)\\s+PAGE\\s+(\\d+)\\s*$")

(define department-line-regexp
  #px"^\\s*0?SCHOOL - 52 ENGINEERING\\s+DEPARTMENT - (.*[^ ])")

(define no-department-line-regexp
  #px"^\\s*0?SCHOOL - 52 ENGINEERING\\s*$")

(define course-header-regexp
  #px"^\\s+COURSE ID   SECT HEGIS LVL ENR  LS CS A-CCU DAYS  BEG  END   TBA  FACL SPACE/TYPE GRP   TTF    SCU    FCH  D-WTU  I-WTU  T-WTU")

(define split-appointment-regexp
  #px"^\\s+\\*\\*\\* SEE PRIMARY SCHOOL/DEPT \\(\\d+/\\d+\\) DETAIL PAGE FOR LIST OF ALL SECTIONS TAUGHT BY THIS SPLIT APPT FACULTY \\*\\*\\*")

(define split-appointment-note-1-regexp
  #px"^\\s+NOTE: D-WTU, I-WTU & T-WTU VALUES IMMEDIATELY BELOW DO NOT INCLUDE CONTRIBUTIONS FROM ASSIGN TIME ACTIVITIES WITHIN 2ND & 3RD")

(define split-appointment-note-2-regexp
  #px"^\\s+DEPARTMENTS\\. ACTUAL VALUES OVER ALL DEPT APPOINTMENTS ARE: D-WTU= *[0-9.]+, I-WTU= *[0-9.]+, T-WTU= *[0-9.]+")


(define pre-2144-regexp-pairings
  (list 
   (list page-header-regexp token-PAGE-HEADER-LINE)
   (list page-assignments-sub-header-regexp token-PAGE-ASSIGNMENTS-LINE)
   (list page-summary-sub-header-regexp token-PAGE-SUMMARY-LINE)
   (list department-line-regexp token-DEPARTMENT-LINE)
   (list no-department-line-regexp token-NO-DEPARTMENT-LINE)
   (list (regexp-quote "  0FACULTY        NUMBER     FTEF     CLASS   SUPERVSN   DIRECT   INDIRECT    TOTAL   DIRECT     TOTAL    TOTAL     TOTAL   SCU/FTEF    SFR ")
         token-SUMMARY-HDR1)
   (list(regexp-quote "    TYPE          ASSIGNMNTS           WTU       WTU       WTU       WTU       WTU   WTU/FTEF  WTU/FTEF    SCU      FTES                    ")
        token-SUMMARY-HDR2)
   (list (regexp-quote "  0FACULTY ID    NAME                          RANGE CODE      TSF         IAF  ADM-LVL        OSF                                 IFF      ")
         token-ASSIGN-HDR1)
   (list (regexp-quote "     SUBJ   COUR SUFF SEC DISC   L ENR    S CS A-CCU DAYS  BEG  END    TBA  FACL SPACE   F  GRP TTF     SCU    FCH  D-WTU I-WTU   T-WTU     ")
         token-ASSIGN-HDR2)
   (list #px"^\\s*ASSIGNED TIME ACTIVITY\\s*$" token-ASSIGN-HDR3)
   (list #px"^[ -]*--[ -]*$" token-INSTRUCTOR-TOTS-DIV)
   (list #px"^[ \\*]*\\*\\*[ \\*]*$" token-INSTRUCTOR-DIV)))

;; oh boy... looks like we need new definitions for 2144 and onward.
(define post-2142-regexp-pairings
  (list
   (list #px"^\\s*\\d+\\s*$" token-STRAY-NUM-LINE)
   (list page-header-2-regexp token-PAGE-HEADER-LINE)
   (list page-header-minimal-regexp token-PAGE-HEADER-MINIMAL-LINE)
   (list page-assignments-sub-header-regexp token-PAGE-ASSIGNMENTS-LINE)
   (list page-summary-sub-header-2-regexp token-PAGE-SUMMARY-2-LINE)
   (list department-line-regexp token-DEPARTMENT-LINE)
   (list no-department-line-regexp token-NO-DEPARTMENT-LINE)
   (list (regexp-quote " FACULTY            NO. OF    APPT     CLASS    SUPERVSN    DIRECT   INDIRECT    TOTAL   DIRECT   TOTAL      TOTAL      TOTAL  SCU/FTEF  SFR")
         token-SUMMARY-HDR1)
   (list(regexp-quote "  TYPE               APPTS    FTEF      WTU        WTU       WTU       WTU        WTU   WTU/FTEF WTU/FTEF     SCU       FTES")
        token-SUMMARY-HDR2)
   (list #px"^\\s+FACULTY ID\\s+NAME\\s+RANGE CODE\\s+TSF\\s+IAF\\s+ADM-LVL\\s+OSF\\s+IFF"
         
         token-ASSIGN-HDR1)
   (list (regexp-quote "     SUBJ   COUR SUFF SEC DISC   L ENR    S CS A-CCU DAYS  BEG  END    TBA  FACL SPACE   F  GRP TTF     SCU    FCH  D-WTU I-WTU   T-WTU     ")
         token-ASSIGN-HDR2)
   (list #px"^\\s*ASSIGNED TIME ACTIVITY\\s*$" token-ASSIGN-HDR3)
   (list (regexp-quote "                                                            TSF    IAF                  OSF                                      IFF")
         token-INSTRUCTOR-HDR)
   (list course-header-regexp
         token-INSTRUCTOR-COURSE-HDR)
   (list split-appointment-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE)
   (list split-appointment-note-1-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1)
   (list split-appointment-note-2-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2)
   (list #px"^[ _]*__[ _]*$" token-INSTRUCTOR-TOTS-DIV)
   (list #px"^[ \\*]*\\*\\*[ \\*]*$" token-INSTRUCTOR-DIV)))

;; oh dear lord... things are different again in 2168?
(define post-2164-regexp-pairings
  (list
   (list #px"^\\s*\\d+\\s*$" token-STRAY-NUM-LINE)
   (list page-header-2-regexp token-PAGE-HEADER-LINE)
   (list page-header-minimal-regexp-post-2164 token-PAGE-HEADER-MINIMAL-LINE)
   (list page-assignments-sub-header-regexp token-PAGE-ASSIGNMENTS-LINE)
   (list page-summary-sub-header-2-regexp token-PAGE-SUMMARY-2-LINE)
   (list department-line-regexp token-DEPARTMENT-LINE)
   (list no-department-line-regexp token-NO-DEPARTMENT-LINE)
   (list (regexp-quote " FACULTY            NO. OF    APPT     CLASS    SUPERVSN    DIRECT   INDIRECT    TOTAL   DIRECT   TOTAL      TOTAL      TOTAL  SCU/FTEF  SFR")
         token-SUMMARY-HDR1)
   (list(regexp-quote "  TYPE               APPTS    FTEF      WTU        WTU       WTU       WTU        WTU   WTU/FTEF WTU/FTEF     SCU       FTES")
        token-SUMMARY-HDR2)
   (list #px"^\\s+SSN\\s+EMPLOYEE ID\\s+NAME\\s+RANGE CODE\\s+TSF\\s+IAF\\s+ADM-LVL\\s+OSF\\s+IFF"
         token-ASSIGN-HDR1)
   (list (regexp-quote "     SUBJ   COUR SUFF SEC DISC   L ENR    S CS A-CCU DAYS  BEG  END    TBA  FACL SPACE   F  GRP TTF     SCU    FCH  D-WTU I-WTU   T-WTU     ")
         token-ASSIGN-HDR2)
   (list #px"^\\s*ASSIGNED TIME ACTIVITY\\s*$" token-ASSIGN-HDR3)
   (list #px"^\\s+TSF\\s+IAF\\s+OSF\\s+IFF\\s*$"
         token-INSTRUCTOR-HDR)
   (list course-header-regexp
         token-INSTRUCTOR-COURSE-HDR)
   (list split-appointment-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE)
   (list split-appointment-note-1-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1)
   (list split-appointment-note-2-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2)
   (list #px"^[ _]*__[ _]*$" token-INSTRUCTOR-TOTS-DIV)
   (list #px"^[ \\*]*\\*\\*[ \\*]*$" token-INSTRUCTOR-DIV)))

(define 2174-summary-header-template
  #<<|
 FACULTY            NO. OF    APPT     CLASS    SUPERVSN    DIRECT   INDIRECT    TOTAL   DIRECT   TOTAL      TOTAL      TOTAL  SCU/FTEF  SFR
  TYPE               APPTS    FTEF      WTU        WTU       WTU       WTU        WTU   WTU/FTEF WTU/FTEF     SCU       FTES
|
  )
(define 2174-regexp-pairings
  (list
   (list #px"^\\s*\\d+\\s*$" token-STRAY-NUM-LINE)
   (list page-header-2-regexp token-PAGE-HEADER-LINE)
   (list page-header-minimal-regexp-post-2164 token-PAGE-HEADER-MINIMAL-LINE)
   (list page-assignments-sub-header-regexp token-PAGE-ASSIGNMENTS-LINE)
   (list page-summary-sub-header-2-regexp token-PAGE-SUMMARY-2-LINE)
   (list department-line-regexp token-DEPARTMENT-LINE)
   (list no-department-line-regexp token-NO-DEPARTMENT-LINE)
   (list (regexp-quote
          (first (regexp-split #px"\n"
                               2174-summary-header-template)))
         token-SUMMARY-HDR1)
   (list (regexp-quote
          (second (regexp-split #px"\n"
                                2174-summary-header-template)))
        token-SUMMARY-HDR2)
   (list #px"^\\s+SSN\\s+EMPLOYEE ID\\s+NAME\\s+RANGE CODE\\s+TSF\\s+IAF\\s+ADM-LVL\\s+OSF\\s+IFF"
         token-ASSIGN-HDR1)
   (list (regexp-quote "     SUBJ   COUR SUFF SEC DISC   L ENR    S CS \
A-CCU DAYS  BEG  END    TBA  FACL SPACE   F  GRP TTF     SCU    FCH  D-WTU I-WTU   T-WTU     ")
         token-ASSIGN-HDR2)
   (list #px"^\\s*ASSIGNED TIME ACTIVITY\\s*$" token-ASSIGN-HDR3)
   (list #px"^\\s+TSF\\s+IAF\\s+OSF\\s+IFF\\s*$"
         token-INSTRUCTOR-HDR)
   (list #px"^  COURSE ID   SECT HEGIS LVL ENR  LS CS \
A-CCU DAYS  BEG  END   TBA  FACL SPACE/TYPE GRP   TTF \
   SCU    FCH  D-WTU  I-WTU  T-WTU"
         token-INSTRUCTOR-COURSE-HDR)
   (list split-appointment-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE)
   (list split-appointment-note-1-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1)
   (list split-appointment-note-2-regexp
         token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2)
   (list #px"^[ _]*__[ _]*$" token-INSTRUCTOR-TOTS-DIV)
   (list #px"^[ \\*]*\\*\\*[ \\*]*$" token-INSTRUCTOR-DIV)))

;; given a format, return the set of regexp pairings to use
(define (fmt->regexp-pairings fmt)
  (match fmt
    ['pre-2144 pre-2144-regexp-pairings]
    ['post-2142 post-2142-regexp-pairings]
    ['post-2164 post-2164-regexp-pairings]
    ['2174-fmt 2174-regexp-pairings]))



;; tokenizing happens on lines.
(define ((page-line-tokenize regexp-pairings) line)
  (let loop ([pairings regexp-pairings])
    (cond 
      [(empty? pairings) 
       (token-OTHER-LINE line)]
      [else
       (match-define (list regexp token-maker) (first pairings))
       (match (regexp-match regexp line)
         [(cons dc fields) (token-maker fields)]
         [#f (loop (rest pairings))])])))



(define pre-2144-page-parser
  (parser
   [src-pos]
   [tokens page-level eof-group]
   [grammar (pages [(page pages) (cons $1 $2)]
                   [() empty])
            (page [(PAGE-HEADER-LINE assignments-page)
                   (assignments-page (second $1) (third $1) (fourth $1)
                                     (first $2) (second $2) (third $2))]
                  [(PAGE-HEADER-LINE summary-page)
                   (summary-page (second $1) (third $1) (fourth $1) 
                                 (first $2) (second $2) (third $2))])
            (assignments-page [(PAGE-ASSIGNMENTS-LINE 
                                DEPARTMENT-LINE
                                ASSIGN-HDR1
                                ASSIGN-HDR2
                                ASSIGN-HDR3
                                non-header-lines) 
                               (list $1 $2 $6)])
            (summary-page [(PAGE-SUMMARY-LINE 
                            DEPARTMENT-LINE
                            SUMMARY-HDR1
                            SUMMARY-HDR2
                            non-header-lines) 
                           (list $1 (map string-trim $2) $5)]
                          [(PAGE-SUMMARY-LINE 
                            NO-DEPARTMENT-LINE
                            SUMMARY-HDR1
                            SUMMARY-HDR2
                            non-header-lines) 
                           (list $1 '(#f) $5)])
            (non-header-lines 
             [(OTHER-LINE non-header-lines) 
              (cons (position-token (token-OTHER-LINE $1)
                                    $1-start-pos
                                    $1-end-pos) 
                    $2)]
             [(INSTRUCTOR-TOTS-DIV non-header-lines) 
              (cons (position-token 
                     (token-INSTRUCTOR-TOTS-DIV $1)
                     $1-start-pos
                     $1-end-pos)
                    $2)]
             [(INSTRUCTOR-DIV non-header-lines)
              (cons (position-token (token-INSTRUCTOR-DIV $1)
                                    $1-start-pos
                                    $1-end-pos)
                    $2)]
             [() empty])]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'page-parser 
                   (~a "tok-ok?:"tok-ok?"\ntok-name: "
                       tok-name"\ntok-value: "(~s tok-value)
                       " start-pos, line: "(position-line start)
                       ", column: "(position-col start)"\n")))]
   [start pages]))

;; what a pain... it's different for 2144 and onward:
(define post-2142-page-parser
  (parser
   [src-pos]
   [tokens page-level eof-group]
   [grammar (pages [(STRAY-NUM-LINE page pages) (cons $2 $3)]
                   [(page pages) (cons $1 $2)]
                   [() empty])
            (page [(PAGE-HEADER-LINE assignments-page)
                   (assignments-page (second $1) (third $1) (fourth $1)
                                     (map string-trim (first $2))
                                     (second $2) (third $2))]
                  [(PAGE-HEADER-MINIMAL-LINE summary-page)
                   (summary-page #f "" (fourth (first $2)) 
                                 (map string-trim
                                      (take (first $2) 3))
                                 (second $2)
                                 (third $2))])
            (assignments-page [(PAGE-ASSIGNMENTS-LINE 
                                DEPARTMENT-LINE
                                ASSIGN-HDR1
                                ASSIGN-HDR3
                                non-header-lines) 
                               (list $1 (map string-trim $2) $5)])
            (summary-page [(PAGE-SUMMARY-2-LINE 
                            DEPARTMENT-LINE
                            SUMMARY-HDR1
                            SUMMARY-HDR2
                            non-header-lines) 
                           (list $1 (map string-trim $2) $5)]
                          [(PAGE-SUMMARY-2-LINE 
                            NO-DEPARTMENT-LINE
                            SUMMARY-HDR1
                            SUMMARY-HDR2
                            non-header-lines) 
                           (list $1 '(#f) $5)])
            (non-header-lines
             [(non-header-line non-header-lines)
              (cons $1 $2)]
             [() empty])
            (non-header-line
             [(OTHER-LINE) 
              (position-token (token-OTHER-LINE $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-TOTS-DIV) 
              (position-token 
               (token-INSTRUCTOR-TOTS-DIV $1)
               $1-start-pos
               $1-end-pos)]
             [(INSTRUCTOR-DIV)
              (position-token (token-INSTRUCTOR-DIV $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-HDR)
              (position-token (token-INSTRUCTOR-HDR $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-COURSE-HDR)
              (position-token (token-INSTRUCTOR-COURSE-HDR $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-SPLIT-APPOINTMENT-LINE)
              (position-token (token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1)
              (position-token (token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1 $1)
                              $1-start-pos
                              $1-end-pos)]
             [(INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2)
              (position-token (token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2 $1)
                              $1-start-pos
                              $1-end-pos)])]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'dept-page-parser 
                   (~a "parse failure. tok-ok?:"tok-ok?" tok-name: "
                       tok-name" tok-value: "(~s tok-value)
                       " start-pos: "
                       (cond [start
                              (format "line ~v, col ~v"
                                      (position-line start)
                                      (position-col start))]
                             [else start]))))]
   [start pages]))

;; string -> (listof page)
(define (file->pages filename format)
  (define parser
    (match format
      ['pre-2144  pre-2144-page-parser]
      [other      post-2142-page-parser]))
  (parser (file->lines-thunk filename format)))

;; export tokens for exploration:
(define (file->tokens filename format)
  (define regexp-pairings
    (fmt->regexp-pairings format))
  (define line->token (page-line-tokenize regexp-pairings))
  (map line->token (file->lines filename)))



;; try it out:
;;(define a (file->pages "fad-2008-fall.txt"))

;;;;;;;;;
;;
;;  SEPARATING INTO DEPARTMENTS
;;
;;;;;;;;;

(define (sanity-checks/pages pages)
  (display (~a "found "(length pages)" pages.\n"))
  
  ;; all pages must have the same date:
  (define dates (remove-duplicates
                 (map page-date (filter assignments-page? pages))))
  (unless (= (length dates) 1)
    (error 'date-check (~a "expected all assignments pages to have the same "
                           "date, got these dates:"dates"\n")))
  
  ;; all pages must have the same number before the hyphen:
  (define page-as (remove-duplicates (map page-page-a pages)))
  (unless (= (length page-as) 1)
    (error 'page-a-check "~a"
           (~a "expected all pages to have the same "
               "page number before the hyphen, got these:"
               page-as"\n")))
  
  ;; page number differences
  (define offsets
    (remove-duplicates (for/list ([p pages] [q (rest pages)])
                         (- (string->number (page-page-b q))
                            (string->number (page-page-b p))))))

  ;; page numbers should be monotonically increasing with one allowed step back...
  (unless (< (length (filter (λ (x) (< x 1)) offsets)) 2)
    (let ([ans (filter (λ (x) (< x 0)) offsets)])
      (printf "~s\n" ans)
      ans)
    (error 'page-b-check (~a "expected all pages to appear in "
                             "increasing order with at most one "
                             "step backward, got: "
                             (map page-page-b pages)"\n")))

  ;; all pages should have the same job/pgm/qtr
  (define jobs (remove-duplicates (map page-labels pages)))
  (unless (= (length jobs) 1)
    (error 'page-job-check (~a "expected all pages to have the "
                               "same job/pgm/quarter , got: "jobs"\n"))))

;; group into department-detail and summary pages.

;; different grouping algorithm, should produce same results.
;; group the pages by department and go from there
(define (pages->dept-list pages)
  (define-values (college-summary-pages other-pages)
    (partition (λ (p) (or (not (first (page-dept p)))
                          (regexp-match #px"^\\s+$"
                                        (first (page-dept p))))) pages))
  (define college-summary-page
    (match college-summary-pages
      [(list p) (parse-summary-page (page-lines p))]
      [(list) 'no-college-summary-page]
      [other (error 'pages->dept-list
                    "expected at most one college summary page")]))
  (define dept-groups (group-by page-dept other-pages))
  (define depts
    (for/list ([group (in-list dept-groups)])
      (define summary-page (last group))
      (unless (summary-page? summary-page)
        (error 'pages->dept-list
               "expected summary as last page of dept ~a, got: ~e\n"
               (page-dept summary-page)
               summary-page))
      (define assignments-pages (take group (sub1 (length group))))
      (unless (andmap assignments-page? assignments-pages)
        (error 'pages->dept-list
               "expected assignments as non-last pages of dept ~a, got page #s: ~e\n"
               (page-dept (last group))
               (map page-page-a assignments-pages)))
      (dept-pages (string-trim (first (page-dept summary-page)))
                  (parse-summary-page (page-lines summary-page))
                  (apply append (map page-lines assignments-pages)))))  
  (fad-pages college-summary-page
             depts))

;; dealing with summary pages

(define expected-summary-labels
  '("FULL TIME" 
     "PART TIME"
     "GRAD ASSISTANT"
     "SUBTOTAL"
     "OTHER"
     "TOTAL"
     "SALARY" "RANGE TITLE"
     "SALARY RANGE TITLE"
     "PROFESSOR/LECT D"
     "ASSOC PROF/LECT C"
     "ASST PROF/LECT B"
     "INSTRUCTOR/LECT A"
     "TCHG ASST/LECT L"
     "TCHNG ASSOCIATE"
     "TCHG ASSOCIATE"
     "GRAD ASSISTANT"
     "ADMINSTRATOR"
     "ADMINISTRATOR"
     "-"))





;; given a string, return a list containing the
;; trimmed first 20 chars and the rest of the line split by spaces
(define (summary-split-line l)
  (list (string-trim (substring l 0 (min 20 (string-length l))))
        (regexp-split #px"\\s+" (substring l (min 20 (string-length l))))))

;; given a summary page, produce an association list
;; from labels to lists of values
(define (parse-summary-page lines)
  (define split (map summary-split-line
                     (map token-value 
                          (map position-token-token 
                               lines))))
  (for ([l (in-list (map first split))])
    (unless (member l expected-summary-labels)
      (error 'parse-summary
             "unexpected label: ~e" l)))
  split)

(define column-titles
  '(#f number ftef class-wtu supervsn-wtu direct-wtu indirect-wtu
       total-wtu direct-wtu-per-ftef wtu-per-ftef total-scu
       total-ftes scu-per-ftef sfr))

;; this table maps column names to indices:
(define column-table
  (for/hash ([title column-titles] [idx (in-naturals)])
    (values title idx)))

(define (summary-column-ref title row)
  (string->number
   (list-ref (second row) 
             (hash-ref column-table title
                       (lambda ()
                         (error (~a "no column named "title)))))))



;; (listof page) -> fad-pages
(define (pages->fad-pages pages)
  (sanity-checks/pages pages)
  (pages->dept-list pages))


(module+ test
  (require rackunit)
  
  (check-equal?
   ((page-line-tokenize pre-2144-regexp-pairings)
    "  03/09/11      CHANCELLOR'S OFFICE              THE CALIFORNIA STATE UNIVERSITIES                               PAGE 15-  197        
")
   (token-PAGE-HEADER-LINE (list "" "03/09/11" "15" "197")))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "MAY 30, 2014                        CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES                    PAGE                 178
")
   (token-PAGE-HEADER-LINE (list "" "MAY 30, 2014" "" "178")))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    " SCHOOL - 52 ENGINEERING                         DEPARTMENT - 112 AERO ENG                                               "
    )
   (token-DEPARTMENT-LINE
    (list "112 AERO ENG")))

  (check-equal?
   ((page-line-tokenize pre-2144-regexp-pairings)
    "  0SCHOOL - 52 ENGINEERING                          DEPARTMENT - 112 AERO ENG                                                               "
    )
   (token-DEPARTMENT-LINE
    (list "112 AERO ENG")))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    " FACULTY ID    NAME                   RANGE CODE            TSF    IAF     ADM-LVL      OSF                                      IFF")
   (token-ASSIGN-HDR1 '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "                                     CHANCELLOR'S OFFICE OF CALIFORNIA STATE UNIVERSITIES")
   (token-PAGE-HEADER-MINIMAL-LINE '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    " JOB APD55   PGM APD60            SUMMARY BY FACULTY TYPE AND RANK FOR SPRING 2016 SAN LUIS OBISPO                                   PAGE  45")
   (token-PAGE-SUMMARY-2-LINE '("55" "60" "SPRING 2016 SAN LUIS OBISPO" "45")))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "  *** SEE PRIMARY SCHOOL/DEPT (52/176) DETAIL PAGE FOR LIST OF ALL SECTIONS TAUGHT BY THIS SPLIT APPT FACULTY ***")
   (token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "   *** SEE PRIMARY SCHOOL/DEPT (52/176) DETAIL PAGE FOR LIST OF ALL SECTIONS TAUGHT BY THIS SPLIT APPT FACULTY ***")
   (token-INSTRUCTOR-SPLIT-APPOINTMENT-LINE '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    " NOTE: D-WTU, I-WTU & T-WTU VALUES IMMEDIATELY BELOW DO NOT INCLUDE CONTRIBUTIONS FROM ASSIGN TIME ACTIVITIES WITHIN 2ND & 3RD")
   (token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1 '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "       DEPARTMENTS. ACTUAL VALUES OVER ALL DEPT APPOINTMENTS ARE: D-WTU=10.0, I-WTU= 0.0, T-WTU=10.0")
   (token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2 '()))

  (check-equal?
   ((page-line-tokenize post-2142-regexp-pairings)
    "       DEPARTMENTS. ACTUAL VALUES OVER ALL DEPT APPOINTMENTS ARE: D-WTU= 7.3, I-WTU= 0.0, T-WTU= 7.3")
   (token-INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2 '()))

  (check-equal?
   (regexp-match?
    #px"^\\s+SSN\\s+EMPLOYEE ID\\s+NAME\\s+RANGE CODE\\s+TSF\\s+IAF\\s+ADM-LVL\\s+OSF\\s+IFF"
    "   SSN        EMPLOYEE ID     NAME                   RANGE CODE            TSF    IAF     ADM-LVL           OSF                                      IFF"
    )
   true)
  (check-equal?
   ((page-line-tokenize post-2164-regexp-pairings)
    "   SSN        EMPLOYEE ID     NAME                   RANGE CODE            TSF    IAF     ADM-LVL           OSF                                      IFF")
   (token-ASSIGN-HDR1 '()))


  (check-equal?
   (regexp-match?
    #px"^[A-Z]+\\s+[0-9]+,\\s+[0-9]+\\s*CHANCELLOR'S OFFICE OF \
CALIFORNIA STATE UNIVERSITIES\\s*$"
    "NOVEMBER 23, 2016                   CHANCELLOR'S OFFICE OF \
CALIFORNIA STATE UNIVERSITIES")
   #t)
  
  (check-equal?
   ((page-line-tokenize post-2164-regexp-pairings)
    "NOVEMBER 23, 2016                   CHANCELLOR'S OFFICE \
OF CALIFORNIA STATE UNIVERSITIES")
   (token-PAGE-HEADER-MINIMAL-LINE '()))

  (check-equal?
   ((page-line-tokenize post-2164-regexp-pairings)
    "                                                                           TSF    IAF                       OSF                                      IFF")
   (token-INSTRUCTOR-HDR '()))



  
  
  )


;; try it out:
;;(define a (file->fad-pages "fad-2008-fall.txt"))

