;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |201510756 박준호 프구해 과제1-2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;과제1
(define (bitfunc x) (+ (- (expt x 4) (* (expt x 2) 5)) 4))
;test case
(bitfunc 0) ;4
(bitfunc 1) ;0

;과제2
(define (bitfunc-rect x1 x2)
  (* (bitfunc x1) (- x2 x1)))
;test case
(bitfunc-rect 0 1) ;4
(bitfunc-rect 1.5 2) ;-1.09375 negative

;과제3
;num-steps = 사각형 갯수
;recursive
(define (bitfunc-integral-recur num-steps x1 x2)
  (if (= num-steps 0) 0
      (+ (abs (* (bitfunc x1) (/ (- x2 x1) num-steps)))
         (bitfunc-integral-recur (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))))
;일단 이프문에 걸리지 않을 때, 전체 범위를 사각형의 갯수로 나누면 밑변이 되므로 (x2 - x1)/ num-steps로 밑변을 구한다.
;그다음 bitfunc로 그때의 높이를 구한 후 밑변과 곱해 사각형의 넓이를 구한다.
;그리고 다음 함수를 부르는데 이때의 x1 과 x2 사이의 사각형의 갯수는 (num-steps - 1)개 이므로 num-steps는 -1 해서 넘겨주고
;x1 은 밑변만큼 진행 해 있어야 하므로 x1 + (x2 - x1)/num-steps)해서 넘겨준다
;마지막으로 num-steps 에 1씩 빼다가 마지막에 0이 됐을때 구할 넓이가 없으므로 0을 리턴하고
;반복이 끝나면 지금까지 나왔던 값들을 모두 더한후 그 결과값을 리턴한다.
;그리고 그래프가 음수값으로 내려갔을 때 넓이가 음수가 나오면 안되므로 abs를 이용하여 절댓값을 리턴해야 한다.
;recur test case
(bitfunc-integral-recur 1 3 6) ; 3~6 까지 사각형이 1개일때 그 사각형의 넓이 120
(bitfunc-integral-recur 4 2 4) ; 2~4 까지 사각형이 4개일때 그 사각형 들의 넓이의 합 72.3125

;iterative
(define (bitfunc-integral-iter num-steps x1 x2) (bitfunc-helper 0 num-steps x1 x2))
(define (bitfunc-helper product num-steps x1 x2)
  (if (= num-steps 0) product
      (bitfunc-helper (abs (+ product (* (bitfunc x1) (/ (- x2 x1) num-steps)))) (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2)))
;모든 넓이의 값은 product에 저장된다. 일단 이프문에 걸리지 않을 때, 바로 bitfunc-helper 를 부른다.
;이때 product는 넓이를 구하고 그 값을 product에 더해서 넘겨주고,
;num-steps는 - 1 을 해서, x1 은 x1 + (x2 - x1)/num-steps) 해서 넘겨준다.
;그후 마지막에 num-steps가 0이 됐을 때 if문에 걸리게 되고, 반복이 종료되며, 지금 까지 넓이를 모두 더해 넣은 produdct를 리턴한다.
;그리고 그래프가 음수값으로 내려갔을 때 넓이가 음수가 나오면 안되므로 abs를 이용하여 절댓값을 리턴해야 한다.
;iter test case
(bitfunc-integral-iter 1 3 6) ; 3~6 까지 사각형이 1개일때. 그 사각형의 넓이 120
(bitfunc-integral-iter 4 2 4) ; 2~4 까지 사각형이 4개일때 그 사각형 들의 넓이의 합 72.3125

;과제4
(define (bitfunc-integral-difference num-steps x1 x2)
  (abs (- (bitfunc-integral-recur num-steps x1 x2) (bitfunc-integral-iter num-steps x1 x2))))
;두 적분기의 차이를 구하는 함수
;test case
(bitfunc-integral-difference 1 4 5) ; 두 적분기의 값 차이 0
(bitfunc-integral-difference 1 4 5) ; 두 적분기의 값 차이 0
