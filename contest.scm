;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <Starry Night>
;;;
;;; Description:
;;;   <Like a shadow in the dark, left behind in this night
;;;    The moon gets brighter
;;;    It's drawing a light.>

(define (draw)
  ; YOUR CODE HERE

  (define (move-pen x y)
      (penup)
      (setposition x y)
      (pendown))

  (define (back s c r g b x y t d)
      (define (helper rad n dx)
      (if (< n t)
        (begin
          (move-pen (- x (* n  c)) y)
          (fill-cir  (rgb  (+ r dx) (+ g dx) (+ b dx))  (- rad (* n  c)))
          (helper rad (+ n 1) (+ d dx)))))
    (helper s 1 d))

  (define (moon)
    (back 250 8 0.071 0.090 0.20 500 200 24 0.01)
    (move-pen 304 200)
    (fill-cir "#7e7481" 54)
    (move-pen 300 200)
    (fill-cir "#e9d9da" 50)

    (move-pen (- 250 10) 270)
	  (star 3)
    (move-pen (- 300 10) 300)
	  (star 3)
    (move-pen (- 220 10) 30)
	  (star 3))

  (define (draw-cir c r)
      (color c)
      (circle r))

  (define (fill-cir c r)
      (begin_fill)
      (draw-cir c r)
      (end_fill))

(define (mount s c col x y)
    (move-pen x (-(+ y (quotient (* (+ s c) 93) 2))))
    (fill-cir (rgb 0.067 0.086 0.16) (* (+ s c) 93))
      
    (define (helper r n)
      (if (< n 93)
        (begin
          (move-pen x (-(+ y (quotient (* r n) 2))))
          (draw-cir  col  (* r n))
          (helper r (+ n 1) ))))
    (helper (+ s c) 1))

(define (line d)
	(right 186)
	(forward d)
	(right 167)
	(forward d)
	(right 7))

(define (star size)
  (begin_fill)
  (define (helper y n)
    (if (< n 8)
    (if y
      (begin
        (line (/ size 2))
        (right 45)
        (helper #f (+ n 1)))
      (begin
      	(line size)
        (right 45)
        (helper #t (+ n 1))))))
  (helper #f 1)
  (end_fill))


(define (dr-star size c x y)
  (back 10 0.5 0.071 0.090 0.20 x y 10 0.01)

  (color c)
  (move-pen (- x 10) y)
	(star size)
  (setheading 0))

(define (drawb n)
    (if (> n 5)
      (begin
        (forward n)
        (right 25)
        (drawb (- n 5))
        (left 50)
        (drawb (- n 5))
        (right 25)
        (backward n))))

(define (tree x y size r dy)
  (color "black")
    (define (helper n y)
      (if (< n r)
        (begin 
          (move-pen (+ x n) y)
          (drawb size)
          (helper (+ 1 n) y))))
    (helper 1 y)
    (helper 1 (+ y dy))
    (helper 1 (- y dy)))


  (bgcolor (rgb 0.071 0.090 0.20))
  (back 150 10 0.071 0.090 0.20 540 (- 170) 20 0.01)
  (tree 390 (- 330) 45 10 20)
  (back 150 10 0.071 0.090 0.20 (- 230) 70 20 0.01)
  (mount 0 10 (rgb  0.5 0.49 0.71) 500 400 )
  (tree (- 390) -15 35 8 7)
  (tree (- 350) -15 35 8 7)
  (mount 0 10 (rgb  0.5 0.49 0.71) 470 500 )
  (mount 0 10 (rgb  0.5 0.49 0.71) 320 450 )
  (mount 0 10 (rgb  0.5 0.49 0.71) (- 70) 510 )

  (moon)
  (dr-star 3 "white" 400 450 )
  (dr-star 5 "white" 480 300 )
  (dr-star 3 "white" 50 50)
  (dr-star 3 "white" -50 90 )
  (dr-star 3 "white" -260 430)
  (dr-star 7 "white" 490 250)

  (dr-star 3 "white" -20 300 )
  (dr-star 4 "white" -18 400 )
  (dr-star 3 "white" 50 480 )
  (dr-star 3 "white" -300 300)
  (dr-star 3 "white" -400 180)
  (dr-star 3 "white" -420 250)
  (dr-star 3 "white" -50 470)

  (dr-star 2 "white" 240 480)
  (dr-star 2 "white" -250 300)
  (dr-star 2 "white" -40 350)
  (dr-star 6 "white" -100 230)
  (dr-star 2 "white" -420 420)
  (dr-star 2 "white" -140 290)
  (dr-star 2 "white" -250 180)

  (hideturtle)
  (exitonclick))
; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)