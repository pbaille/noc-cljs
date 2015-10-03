(ns noc.utils.elements
  (:require [noc.utils.pv2 :refer :all]
            [quil.core :refer :all]
            [quil.middleware :as m]))

(defn show-default [{[x y] :loc}]
  (rect x y (height) (width)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn print-default [{:keys [loc vel]}]
  (println "loc" (map (partial round2 2) loc)
           "vel" (map (partial round2 2) vel)))

(defn apply-force [{:keys [vel mass] :as m} f]
  (assoc m :vel (add vel (div f mass))))

(defn mover-step [m]
  (assoc m :loc (add (:loc m) (:vel m))))

(defn get-older [m]
  (update-in m [:age] inc))

(defn element [opts]
  (merge {:loc   v0
          :vel   v0
          :mass  1
          :show  show-default
          :step  (comp get-older mover-step)
          :act   (fn [_ x] x)
          :act?  (fn [_ _] true)
          :age   0
          :print print-default}
         opts))

(defn step [x] ((:step x) x))
(defn act [x y] ((:act x) x y))
(defn act? [x y] ((:act? x) x y))
(defn show [x] ((:show x) x))
(defn p [x] ((:print x) x))

(defn do-interactions [xs]
  (reduce (fn [es [i e]]
            (let [f (fn [y] (if (act? e y) (act e y) y))]
              (concat (map f (take i es)) [(nth es i)] (map f (drop (inc i) es)))))
          xs
          (map-indexed vector xs)))

(defn elements [& xs]
  {:xs   xs
   :step (fn [x] (update-in x [:xs] (fn [xs] (map step (do-interactions xs)))))
   :show (fn [x] (mapv show (:xs x)))})



(do

  (def G 1)

  (defn distant-interaction [sign limit]
    (fn [x y]
      (let [dist (sub (:loc x) (:loc y))
            ma (magn dist)
            fma (/ (* G (:mass x) (:mass y)) (* ma ma))
            f (-> dist normalize (mult (sign fma)) (lim limit))]
        (update-in y [:vel] #(add % f)))))

  (def gravitation (distant-interaction - 20))
  (def repulsion (distant-interaction + 20))

  (def movers
    (map
      (fn [_]
        (element
          {:vel  [(rand 1) (rand 1)]
           :loc  [(rand 500) (rand 500)]
           :show (fn [{[x y] :loc}] (fill 255 255 255) (ellipse x y 50 50))
           :mass (rand-nth (range 0.5 2 0.25))
           :act  gravitation
           :age  (int (* 1000 (rand)))}))
      (range 10)))

  (def space
    (element
      {:mass 10000
       :show (fn [_] (background 125))
       :act  (fn [x y] (apply-force y [0 0.01]))}))

  (def borders
    (element
      {:mass  10000
       :show  (fn [_] ())
       :step  (fn [x] x)
       :fixed true
       :act   (fn [b {[x y] :loc :as t}]
                (let [closestx (if (> (abs (- 500 x)) (abs x)) 500 0)
                      closesty (if (> (abs (- 500 y)) (abs y)) 500 0)]
                  #_(println "bact" closestx closesty)
                  (repulsion {:loc [closestx closesty] :mass 10000} t)))}))

  #_(act borders (first movers))

  (defn setup []
    (no-stroke)
    (stroke-weight 10)
    (background 0)
    #_(elements borders (first movers))
    (apply elements space borders movers))

  (defsketch ex
             :size [500 500]
             :setup setup
             :draw show
             :update step
             ; :mouse-moved mouse-moved
             :middleware [m/fun-mode]))






