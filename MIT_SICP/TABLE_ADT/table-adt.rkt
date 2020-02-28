#lang racket
(require r5rs)
(require rackunit)
(print-as-expression #f)
(print-mpair-curly-braces #f)

;find-assoc
(define (find-assoc key alist)
  (cond ((null? alist) #f)
	((equal? (caar alist) key) (cadar alist))
	(else (find-assoc key (cdr alist)))))
;add-assoc
(define (add-assoc key val alist)
  (cons (list key val) alist))

;make-point
(define (make-point x y)
  (cons x y))
;hashfunc
(define (hashfunc x y)
  (+ (modulo (+ (car x) (cdr x)) y) 1))

;make-bucket
(define (make-bucket)
  (list 'bucket))
;bucket
(define bucket (list 'bucket))

;make-vector
(define (make-vector size value)
  (if (not (= size 0))
      (and (set-cdr! bucket (add-assoc size value (cdr bucket)))
           (make-vector (- size 1) value)) bucket))
;vector-ref
(define (vector-ref vector index)
  (cond ((null? (cdr vector)) (error "ERROR"))
	((equal? (caar (cdr vector)) index) (car (cdr vector)))
	(else (vector-ref (cdr vector) index))))

;vector-set!
(define (vector-set! vector index val)
  (cond ((null? (cdr vector)) (error "ERROR"))
	((equal? (caar (cdr vector)) index) (set-cdr! (car (cdr vector)) val))
	(else (vector-set! (cdr vector) index val))))

;bucket-abstraction
(define (make-buckets N v) (make-vector N v))
(define bucket-ref vector-ref)
(define bucket-set! vector-set!)

;table-element
(define (size-of tbl) (cadr tbl))
(define (hashfunc-of tbl) (caddr tbl))
(define (buckets-of tbl) (cadddr tbl))

;make-table
(define table-tag 'hash-table)
(define (make-table size hashfunc)
  (let ((buckets (make-buckets size null)))
    (list table-tag size hashfunc buckets)))

;table-put
(define (table-put! tbl key val)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (bucket-set! buckets index (cons key val))))

;table-get
(define (table-get tbl key)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (bucket-ref buckets index)))

(define hash-table (make-table 4 hashfunc))
(table-put! hash-table (make-point 5 5) 20)
(table-put! hash-table (make-point 5 7) 15)
(table-get hash-table (make-point 5 5))
