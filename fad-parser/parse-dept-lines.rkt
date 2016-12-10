#lang racket

;; functions to parse individual instructor lines

(require "fad-to-pages.rkt"
         parser-tools/lex
         parser-tools/yacc
         racket/generator)


(provide pre-2144-dept-lines-parser
         post-2142-dept-lines-parser)



;; group by instructor:

(define (pre-2144-dept-lines-parser lines dept-name)
  ((parser
   [src-pos]
   [tokens page-level eof-group]
   [grammar 
    (instructors [(instructor instructors) (cons $1 $2)]
                 [() empty])
    (instructor [(other-lines INSTRUCTOR-TOTS-DIV OTHER-LINE INSTRUCTOR-DIV)
                 (list $1 $3)])
    (other-lines [(OTHER-LINE other-lines) (cons $1 $2)]
                 [() empty])]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'dept-line-parser 
                   (~a "error while parsing department lines for department "
                       dept-name".\ntok-ok?:"tok-ok?"\ntok-name: "
                       tok-name"\ntok-value: "(~s tok-value)
                       " start-pos, line: "(position-line start)
                       ", column: "(position-col start)"\n")))]
   [start instructors])
   (lines->token-feed lines)))

(define (post-2142-dept-lines-parser lines dept-name)
  ((parser
   [src-pos]
   [tokens page-level eof-group]
   [grammar
    ;; oh for heaven's sake... last instructor doesn't have stars after it any more, but
    ;; only on the last page before the summaries....
    (instructors [(instructor instructors) (cons $1 $2)]
                 [(instructor-no-stars) (list $1)]
                 [(instructor) (list $1)])
    (instructor
     [(instructor-no-stars INSTRUCTOR-DIV) $1])
    (instructor-no-stars
     [(INSTRUCTOR-HDR OTHER-LINE other-lines INSTRUCTOR-COURSE-HDR other-lines INSTRUCTOR-TOTS-DIV OTHER-LINE)
      (list 'home-dept $2 $3 $5 $7)]
     [(INSTRUCTOR-HDR OTHER-LINE other-lines
                      INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-1
                      INSTRUCTOR-SPLIT-APPOINTMENT-NOTE-2
                      INSTRUCTOR-COURSE-HDR other-lines INSTRUCTOR-TOTS-DIV OTHER-LINE)
      (list 'home-dept $2 $3 $7 $9)]
     [(INSTRUCTOR-HDR OTHER-LINE other-lines INSTRUCTOR-SPLIT-APPOINTMENT-LINE INSTRUCTOR-TOTS-DIV OTHER-LINE)
      (list 'split-appt-remote $2 $3 $6)]
     [(INSTRUCTOR-HDR OTHER-LINE other-lines INSTRUCTOR-TOTS-DIV OTHER-LINE)
      (list 'no-class-instructor $2 $3 $5)])
    (other-lines [(OTHER-LINE other-lines) (cons $1 $2)]
                 [() empty])]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'dept-line-parser 
                   (~a "error while parsing department lines for department "
                       dept-name".\ntok-ok?:"tok-ok?"\ntok-name: "
                       tok-name"\ntok-value: "(~s tok-value)
                       " start-pos, line: "(and start (position-line start))
                       ", column: "(and start (position-col start))"\n")))]
   [start instructors])
   (lines->token-feed lines)))



;; given a list of lines, return a thunk that returns tokens
;; from those lines
(define (lines->token-feed lines)
  (define generator (sequence->generator lines))
  (lambda ()
    (define next-tok (generator))
    (cond [(void? next-tok) (position-token (token-EOF)
                                            #f #f)]
          [else next-tok])))