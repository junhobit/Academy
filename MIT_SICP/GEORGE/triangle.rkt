
#lang racket
(require racket/gui/base)
(require racket/draw)

;vector
(define (make-vect x y) (cons x y))
(define (xcor vect) (car vect))
(define (ycor vect) (cdr vect))

;segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;triangle
(define t1 (make-vect 0 0))
(define t2 (make-vect 0 1))
(define t3 (make-vect 1 0))

;triangle-vect
(define triangle-vect
  (list
   (make-segment t1 t2)
   (make-segment t2 t3)
   (make-segment t3 t1)))

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

;triangle
(define triangle (make-picture triangle-vect))


;rotate pict
(define (rotate pict angle)
  (lambda (rect)
    (let* ((a (make-rectangle
               (+vect (origin rect)
                      (y-axis rect))
               (scale-vect (y-axis rect) -1)
               (x-axis rect)))
           (b (make-rectangle
               (+vect (origin a)
                      (y-axis a))
               (scale-vect (y-axis a) -1)
               (x-axis a)))
           (c (make-rectangle
               (+vect (origin b)
                      (y-axis b))
               (scale-vect (y-axis b) -1)
               (x-axis b))))
      (cond ((= 1 (/ angle (* 0.5 pi))) (pict a))
            ((= 2 (/ angle (* 0.5 pi))) (pict b))
            ((= 3 (/ angle (* 0.5 pi))) (pict c))))))

          
;above-rotate45
(define (above-rotate45 pict)
  (lambda (rect)
    (pict (make-rectangle
           (origin rect)
           (+vect (scale-vect (x-axis rect) 0.5)
                  (scale-vect (y-axis rect) -0.5))
           (+vect (scale-vect (x-axis rect) 0.5)
                  (scale-vect (y-axis rect) 0.5))))))


;screen-transform
(define (screen-transform pict)
   (lambda (rect)
     (rotate180 (flip pict))))

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
                  (y-axis rect))
           (scale-vect (y-axis rect) -1)
           (x-axis rect)))))

;180rotate
(define rotate180 (repeated rotate90 2))

;flip
(define (flip pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect)
                  (y-axis rect))
           (x-axis rect)
           (scale-vect (y-axis rect) -1)))))

;empty
(define empty-picture (make-picture null))

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


;together4
(define (together4 pict1 pict2 pict3 pict4)
  (lambda (rect)
    (pict1 rect) (pict2 rect) (pict3 rect) (pict4 rect)))
(define triangle2 (above-rotate45 (flip triangle)))
(define triangle3 (rotate triangle2 (* 0.5 pi)))
(define triangle4 (rotate triangle2 (* 1.0 pi)))
(define triangle5 (rotate triangle2 (* 1.5 pi)))
(define triangle-tile (together4 triangle2 triangle3 triangle4 triangle5))


;quardtet
(define (quardtet p q r s)
  (beside
   (above p r .5)
   (above q s .5)
   .5))
(define b (quardtet triangle2 triangle3 triangle4 triangle5))


;noent
(define (nonet p q r s t u v w x)
  (beside
   (beside
    (above p
     (above s v (/ 1 2))
     (/ 1 3))
    (above q
     (above t w (/ 1 2))
      (/ 1 3))
    (/ 1 2))
   (above r
    (above u x (/ 1 2))
     (/ 1 3))
   (/ 2 3)))

;side-push
(define (side-push pict n)
  (if (<= n 0) empty-picture
      (quardtet (side-push pict (- n 1)) (side-push pict (- n 1))
                (rotate pict (* 0.5 pi)) pict)))

;corner-push
(define (corner-push pict n)
  (if (<= n 0) empty-picture
      (quardtet (side-push pict (- n 1)) (corner-push pict (- n 1))
                pict (rotate (side-push pict (- n 1)) (* 1.5 pi)))))

;push triangle
(define triangle-side-push (side-push triangle-tile 2))
(define triangle-corner-push (corner-push triangle-tile 2))

;square-limit
(define (square-limit pict n)
      (nonet (rotate (corner-push pict n) (* 0.5 pi)) (side-push pict n) (corner-push pict n)
             (rotate (side-push pict n) (* 0.5 pi)) pict (rotate (side-push pict n) (* 1.5 pi))
             (rotate (corner-push pict n) (* 1.0 pi)) (rotate (side-push pict n) (* 1.0 pi))
             (rotate (corner-push pict n) (* 1.5 pi))))

(define triangle-square-limit (square-limit triangle-tile 2))



;paint
(define frame (new frame% [label "Paint ␣ Triangle"]
                   [width 747]
                   [height 769]))
(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (cnavas dc)
                       (send dc set-pen red-pen)
                       (send dc set-brush no-brush)
                       (send dc set-smoothing 'smoothed)
                       (on-paint))]))
(define red-pen (make-object pen% "RED" 2 'solid))
(define no-brush (make-object brush% " BLACK " ' transparent))
(define dc (send canvas get-dc))

; DEFINE CALLBACK PAINT PROCEDURE
(define (on-paint) (triangle-square-limit frame1))
;triangle-corner-push
;triangle-square-limit
; MAKING THE FRAME VISIBLE
(send frame show #t)
