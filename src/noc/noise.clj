(ns noc.noise
  (:require [quil.core :refer :all]
            [quil.middleware :as m]))


()

(defn draw []
  (let [n1 (noise (+ 1000 (/ (frame-count) 50)))
        n2 (* 500 (noise (/ (frame-count) 100)))
        x (* (- 1000 (/ n2 2)) (noise (+ 2000 (/ (frame-count) 50))))
        y (* (- 1000 (/ n2 2)) (noise (+ 4000 (/ (frame-count) 50))))]
    (background 255)
    (fill (* 255 n1) 0 0)
    (ellipse x y n2 n2)))


; 'setup' is a cousin of 'draw' function
; setup initialises sketch and it is called only once
; before draw called for the first time
(defn setup []
  ; draw will be called 60 times per second
  (frame-rate 100)
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

