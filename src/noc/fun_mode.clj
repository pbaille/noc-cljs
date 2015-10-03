(ns noc.fun-mode
  (:require [quil.core :refer :all]
            [quil.middleware :as m]
            [noc.utils.pv2 :refer :all]))

(defn setup []
  (stroke 125)
  (stroke-weight 10)
  {:loc     [250 250]
   :vel     [0 0]
   :acc     [0.1 0.2]
   :max-vel 10})

(defn collision-handler [{[x y :as v] :loc :as state}]
  (let [w (width) h (height)]
    (cond
      (> x w) (assoc state :loc (setx v 0))
      (> 0 x) (assoc state :loc (setx v w))
      (> y h) (assoc state :loc (sety v 0))
      (> 0 y) (assoc state :loc (sety v h))
      :else state)))

(defn update [{a :acc v :vel l :loc :as state}]
  ; increase radius of the circle by 1 on each frame
  (let [vel (lim (add v a) 6)
        loc (add l vel)]
    (collision-handler
      (assoc state :vel vel :loc loc))))

(defn draw [{[x y] :loc}]
  (background 190)
  (ellipse x y 25 25))

#_(defn mouse-moved [state event]
    (-> state
        ; set circle position to mouse position
        (assoc :x (:x event) :y (:y event))
        ; decrease radius
        (update-in [:r] shrink)))

(defsketch example
           :size [500 500]
           :setup setup
           :draw draw
           :update update
           ; :mouse-moved mouse-moved
           :middleware [m/fun-mode])