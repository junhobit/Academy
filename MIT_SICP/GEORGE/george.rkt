#lang racket
(require racket/gui/base)

;vector
(define (make-vect x y) (cons x y))
(define (xcor vect) (car vect))
(define (ycor vect) (cdr vect))

;segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;george-vects
(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))
(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))
(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect 1 .15))
(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))
(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))

;george-lines
(define george-lines
  (list (make-segment p1 p2)
        (make-segment p2 p3)
        (make-segment p3 p4)
        (make-segment p4 p5)
        ;(make-segment p1 p6)
        ;(make-segment p5 p22)
        (make-segment p6 p7)
        (make-segment p7 p8)
        ;(make-segment p8 p9)
        (make-segment p9 p10)
        (make-segment p10 p11)
        ;(make-segment p11 p12)
        (make-segment p12 p13)
        (make-segment p13 p14)
        (make-segment p14 p15)
        (make-segment p15 p16)
        ;(make-segment p16 p17)
        (make-segment p17 p18)
        (make-segment p18 p19)
        (make-segment p19 p20)
        (make-segment p20 p21)
        (make-segment p21 p22)
        ))

;+vect
(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
             (+ (ycor v1) (ycor v2))))

;scale-vect
(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
             (* factor (ycor vect))))

;coord-map
(define (coord-map rect)
  (lambda (p)
    (+vect (origin rect)
           (+vect (scale-vect (x-axis rect) (xcor p))
                  (scale-vect (y-axis rect) (ycor p)))
           )))

;rectangle origin
(define make-rectangle list)
(define origin first)
(define x-axis second)
(define y-axis third)

;rectangle set
(define origin1 (make-vect 0 0))
(define x-axis1 (make-vect 730 0))
(define y-axis1 (make-vect 0 730))
(define frame1 (make-rectangle origin1 x-axis1 y-axis1))

;make-picture
(define (make-picture seglist)
  (lambda (rect)
     (for-each
        (lambda (segment)
              (let* ((b (start-segment segment))
                     (e (end-segment segment))
                     (m (coord-map rect))
                     (b2 (m b))
                     (e2 (m e)))
                (send dc draw-line (xcor b2) (ycor b2)
                      (xcor e2) (ycor e2))))
        seglist)))

;george
(define george (make-picture george-lines))


;repeated
(define (repeated f n)
  (if (= n 1)
      f
      (compose
       f (repeated f (- n 1)))))
;rotate
(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect)
                  (x-axis rect))
           (y-axis rect)
           (scale-vect (x-axis rect) -1)))))
;rotate180
(define rotate180 (repeated rotate90 2))
;flip
(define (flip pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect)
                  (x-axis rect))
           (scale-vect (x-axis rect) -1)
           (y-axis rect)))))
;screen-transform
(define (screen-transform pict)
  (lambda (rect)
    (rotate180 (flip pict))))
;george-trans
(define george-trans ((screen-transform george) george))


;beside
(define (beside pict1 pict2 a)
  (lambda (rect)
    (pict1
     (make-rectangle
      (origin rect)
      (scale-vect (x-axis rect) a)
      (y-axis rect)))
    (pict2
     (make-rectangle
      (+vect
       (origin rect)
       (scale-vect (x-axis rect) a))
      (scale-vect (x-axis rect) (- 1 a))
      (y-axis rect)))))
;above
(define (above pict1 pict2 a)
  ((repeated rotate90 3)
   (beside (rotate90 pict1)
           (rotate90 pict2) a)))
;together
(define (together pict1 pict2)
  (lambda (rect)
    (pict1 rect)
    (pict2 rect)))
;acrobats
(define acrobats
  (beside george
          (rotate180 (flip george))
          .5))
;4bats
(define 4bats
  (above acrobats
         (flip acrobats)
         .5))
;up-push
(define (up-push pict n)
  (if (= n 0)
      pict
      (above (up-push pict (- n 2))
             pict
             .25)))
;right-push
(define (right-push pict n)
  (if (= n 0)
      pict
      (beside pict
              (right-push pict (- n 2))
              .75)))
;corner-push
(define (conner-push pict n)
  (if (= n 0)
      pict
      (above
       (beside
        (up-push pict n)
        (conner-push pict (- n 2))
        .75)
       (beside
        pict
        (right-push pict (- n 2))
        .75)
       .25)))
;4pick
(define (4pict p1 r1 p2 r2 p3 r3 p4 r4)
  (flip
   (beside
    (above
     ((repeated rotate90 r1) p1)
     ((repeated rotate90 r2) p2)
     .5)
    (above
     ((repeated rotate90 r3) p3)
     ((repeated rotate90 r4) p4)
     .5)
    .5)))

;4same
(define (4same p r1 r2 r3 r4)
  (4pict p r1 p r2 p r3 p r4))

; square-limit
(define (square-limit pict n) (4same (conner-push pict n) 1 2 4 3))

(define george-squarelimit (square-limit 4bats 2))



;paint
(define frame (new frame% [label "Paint ␣ Triangle"]
                   [width 747]
                   [height 769]))
(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (cnavas dc)
                       (send dc set-pen red-pen)
                       (send dc set-brush no-brush)
                       (on-paint))]))
(define red-pen (make-object pen% "RED" 2 'solid))
(define no-brush (make-object brush% " BLACK " ' transparent))
(define dc (send canvas get-dc))

; DEFINE CALLBACK PAINT PROCEDURE
(define (on-paint) (george-squarelimit frame1))

; MAKING THE FRAME VISIBLE
(send frame show #t)
