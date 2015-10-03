(ns noc.ex1-4
  (:require [quil.core :refer :all]
            [quil.middleware :as m]))

#_(p-step 0 110 3)

(defn h [dispersion med-size size-disp]
  (let [rx (random-gaussian)
        ry (random-gaussian)
        x (+ (/ (width) 2) (* rx dispersion))
        y (+ (/ (height) 2) (* ry dispersion))
        center-distx (abs (- x 500))
        center-disty (abs (- y 500))
        largest-center-dist (/ (max center-distx center-disty) 250)
        s (+ (- med-size (* med-size largest-center-dist)) (* (random-gaussian) (- size-disp (* size-disp largest-center-dist))))]
    (ellipse  x y s s)))

(defn draw []
  (let [r (+ 230 (* (random-gaussian) 25))
        g (+ 10 (* (random-gaussian) 10))
        b (+ 10 (* (random-gaussian) 10))
        a (+ 40 (* (random-gaussian) 40))
        a2 (+ 40 (* (random-gaussian) 40))]
    (fill 255 2)
    (rect 0 0 1000 1000)
    (fill r g b a)
    (h 100 50 40)
    (fill 255 255 255 a2)
    (h 100 50 40)))


; 'setup' is a cousin of 'draw' function
; setup initialises sketch and it is called only once
; before draw called for the first time
(defn setup []
  ; draw will be called 60 times per second
  (frame-rate 200)
  (no-stroke)
  (fill 20 20 200 10)
  (stroke-weight 1)
  (smooth)
  ; set background to white colour only in the setup
  ; otherwise each invocation of 'draw' would clear sketch completely
  (background 255))

(defsketch noc
           :size [1000 1000]
           ;:update update
           :setup setup
           :draw draw)
