#lang racket

(require rackunit)
;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


"Problem 1"
;find-assoc
(define (find-assoc key alist)
  (cond ((null? alist) #f)
	((equal? (caar alist) key) (cadar alist))
	(else (find-assoc key (cdr alist)))))
;add-assoc
(define (add-assoc key val alist)
  (cons (list key val) alist))
;make-table
(define (make-table)
  (list 'table))
;table?
(define (table? table)
  (and 
   (and (list? table)(not (equal? table '()))) (equal? (car table) 'table)))
;table-put!
(define (table-put! table key val)
  (if (table? table)
      (set-cdr! table (add-assoc key val (cdr table)))
      (error "error")))
;table-has-key?
(define (table-has-key? table key)
  (let ((record (find-assoc key (cdr table))))
    (if record	#t #f)))
;table-get
(define (table-get table key)
  (if (table? table)
      (if (find-assoc key (cdr table))
          (find-assoc key (cdr table)) (error "ERROR")) (error "ERROR")))


;test-case
;(define my-table (make-table))
;(table? my-table) ;; => #t
;(table-put! my-table 'ben-bitdiddle 'chocolate) ;; => undefined
;(table-put! my-table 'alyssa-p-hacker 'cake) ;; => undefined
;(table-has-key? my-table 'ben-bitdiddle) ;; => #t
;(table-has-key? my-table 'louis-reasoner) ;; => #f
;(table-get my-table 'ben-bitdiddle) ;; => chocolate
;(table-get my-table 'louis-reasoner) ;; => ERROR

"Problem 2"
;fibonacci
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
;monitord
(define (make-monitored func)
  (let ((count 0))
    (lambda (label)
      (cond ((eq? label 'reset-call-count) (set! count 0))
            ((eq? label 'how-many-calls?) count)
            (else (and (set! count (+ 1 count))
                         (func label)))))))

;test-case
;(fib 8) ;; => 21
;(set! fib (make-monitored fib))
;(fib 8) ;; => 21
;(fib 'how-many-calls?) ;; => 67
;(fib 8) ;; => 21
;(fib 'how-many-calls?) ;; => 134
;(fib 'reset-call-count)
;(fib 'how-many-calls?) ;; => 0



"Problem 3"
;; make-num-calls-table
(define (make-num-calls-table func val)
   (define fib-table (make-table))
   (define (iter count min)
     (func count)
     (table-put! fib-table count (func 'how-many-calls?))
     (func 'reset-call-count)
    (if (equal? count min) (display fib-table)
       (iter (- count 1) min)))
   (iter val 1))

;(make-num-calls-table fib 10)
;(make-num-calls-table fib 20)
;(make-num-calls-table fib 30)

"Problem 4"
;; memoize
(define (memoize func) 
  (define memo-table (make-table))
  (define (fib-put val)
    (table-put! memo-table val (func val)))
  (lambda (val) 
    (cond ((table-has-key? memo-table val) (table-get memo-table val))
           (else (and (fib-put val) (func val))))))
   
;(set! fib (memoize fib))
;(fib 8)


"Problem 5 (optional)"
;; advise
(define (advise func before after)
  (lambda (val) 
    (let ((result 0))
      (before)
      (set! result (func val))
      (after)
      result)))
                 
;(define (add-1 x) (+ x 1))
;(define advised-add-1
 ; (advise add-1
  ;        (lambda () (displayln "calling add-1"))
   ;       (lambda () (displayln "add-1 done"))))

;(advised-add-1 5)
;calling add-1
;add-1 done
;6

"Problem 6 (optional)"
;; make-monitored-with-advice
(define (make-monitored-with-advice func)
  (let ((count 0)
        (num-steps 0))
    (advise func 
            (lambda () (set! count (+ count 1))
              (set! num-steps (+ num-steps 1)))
            (lambda () (set! num-steps (- num-steps 1))
              (if (equal? num-steps 0)
                  (and (and (and (display "Num calls: ")(display count)) (newline))
                       (set! count 0)) void)))))
            
;(set! fib (make-monitored-with-advice fib))
;(fib 10)
;Num calls: 177
;55

;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))