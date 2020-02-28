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
(define (add-assoc key key2 val alist)
  (cons (list key key2 val) alist))

;make-point
(define (make-point x y)
  (cons x y))
;hashfunc
(define (hashfunc x y) void)

;bucket
(define (make-bucket tag) (list tag))

;make-vector
(define (make-vector size value)
  (make-bucket 'bucket))

;vector-ref
(define (vector-ref vector index key)
  (cond ((null? (cdr vector)) #f)
	((equal? (caar (cdr vector)) index)
         (if (equal? (cadr (car (cdr vector))) key) (car (cdr vector))
             (vector-ref (cdr vector) index key)))
	(else (vector-ref (cdr vector) index key))))

;vector-set!
(define (vector-set! vector index key key2 val)
  (if (list? vector)
      (set-cdr! vector (add-assoc key key2 val (cdr vector)))
      (error "error")))

;bucket-abstraction
(define (make-buckets N v) (make-vector N v))
(define bucket-ref vector-ref)
(define bucket-set! vector-set!)

;make-table
(define (make-table table-tag size hashfunc)
  (let ((buckets (make-buckets size null)))
    (list table-tag size hashfunc buckets)))

;table-element
(define (size-of tbl) (cadr tbl))
(define (hashfunc-of tbl) (caddr tbl))
(define (buckets-of tbl) (cadddr tbl))

;table-put
(define (table-put! tbl key key2 val)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (bucket-set! buckets index key key2 val)))

;table-get
(define (table-get tbl key key2)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (bucket-ref buckets key key2)))

;global-table
(define global-table (make-table 'global-table 12 hashfunc))

;square
(define (square x) (* x x))

;attach-type
(define (attach-type type-tag contents)
  (cons type-tag contents))

;type
(define (type datum) (car datum))
(define (contents datum) (cdr datum))

;real, imag part
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;rectangle
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))
(define (angle-rectangular z)
  (atan (cdr z) (car z)))
(define (make-complex-rectangular x y) (attach-type 'complex (cons x y)))

;polar
(define (make-polar r a)
  (attach-type 'polar (cons r a)))
(define (real-part-polar z)
  (* (car z) (cos (cdr z))))
(define (imag-part-polar z )
  (* (car z) (sin (cdr z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-complex-polar r a)
  (attach-type 'complex (cons (real-part-polar (cons r a))
                              (imag-part-polar (cons r a)))))

;make~
(define (make-complex z) (attach-type 'complex z))
(define (make-rat x y) (attach-type 'rational (cons x y)))
(define (make-scheme-number x) (attach-type 'scheme-number x))

;complex-install
(define (install-complex-package)
  (define (+complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (*complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  (define (complex*rational z1 z2)
    (cons (make-rat (* (real-part z1) (real-part z2)) (imag-part z2))
          (cons (make-rat (* (imag-part z1) (real-part z2)) (imag-part z2)) '())))
  (define (complex+rational z1 z2)
    (cons (make-rat (+ (* (imag-part z2) (real-part z1)) (real-part z2))
                (imag-part z2)) 
          (imag-part z1)))
  
  (define (complex*number z1 z2)
    (cons (* z2 (real-part z1)) (* z2 (imag-part z1))))
  (define (complex+number z1 z2)
    (cons (+ z2 (real-part z1)) (imag-part z1)))
  (define (tag z) (attach-type 'complex z))
  
  (put 'MUL '(complex rational)
       (lambda (z1 z2) (tag (complex*rational z1 z2)))) 
  (put 'ADD '(complex rational)
       (lambda (z1 z2) (tag (complex+rational z1 z2))))
  (put 'MUL '(complex scheme-number)
       (lambda (z1 z2) (tag (complex*number z1 z2))))
  (put 'ADD '(complex scheme-number)
       (lambda (z1 z2) (tag (complex+number z1 z2))))
  (put 'MUL '(complex complex)
       (lambda (z1 z2) (tag (*complex z1 z2))))
  (put 'ADD '(complex complex)
       (lambda (z1 z2) (tag (+complex z1 z2))))
  'done)

;scheme-number-install
(define (install-scheme-number-package)
  (define (tag x)
    (attach-type 'scheme-number x))  
  (put 'MUL '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'ADD '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  'done)

;rational-instrall
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (rat+number x y)
    (make-rat (+ (* y (denom x)) (numer x))
              (denom x)))
  (define (rat*number x y)
    (make-rat (* y (numer x))
              (denom x)))
  (define (tag x) (attach-type 'rational x))
  (put 'MUL '(rational scheme-number)
       (lambda (x y) (tag (rat*number x y))))
  (put 'ADD '(rational scheme-number)
       (lambda (x y) (tag (rat+number x y))))
  (put 'MUL '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'ADD '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  'done)

;put, get
(define (put type label val)
  (table-put! global-table type label val))
(define (get key key2)
  (table-get global-table key key2))

;ADD, MUL
(define (ADD x y) (apply-generic 'ADD x y))
(define (MUL x y) (apply-generic 'MUL x y))

;apply-generic
(define (apply-generic label x y)
  (let* ((x-type (car x))
         (y-type (car y))
         (cal-type1 (list x-type y-type))
         (cal-type2 (list y-type x-type)))
    (if (get label cal-type1) ((caddr (get label cal-type1)) (cdr x) (cdr y))
        (if (get label cal-type2) ((caddr (get label cal-type2)) (cdr y) (cdr x))
            (error "ERROR")))))

;install         
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

;test-case
(define c1 (make-complex-rectangular 3 5))
(define c2 (make-complex-rectangular 1 2))
(define c3 (make-complex-polar 3 5))
(define c4 (make-complex-polar 1 2))
(define r1 (make-rat 3 5))
(define r2 (make-rat 5 6))
(define n1 (make-scheme-number 5))
(define n2 (make-scheme-number 8))
(define n3 (list'scheme 3))

(ADD c1 c2)
(ADD c1 c4)
(MUL c1 c4)
(MUL c3 c4)
(ADD r1 r2)
(MUL r1 r2)
(ADD n1 n2)
(MUL n1 n2)
(ADD c1 r1)
(MUL c3 r1)
(MUL c3 n2)
(MUL r2 n2)
