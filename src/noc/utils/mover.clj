(ns noc.utils.mover
  (:require [noc.utils.pv2 :refer :all]
            [quil.core :refer :all]
            [quil.middleware :as m]))

(defn show-default [{[x y] :loc}]
  (ellipse x y 50 50))

(defn apply-forces [{:keys [vel mass] :as m} & [fs]]
  (assoc m :vel (reduce add vel (map #(div % mass) fs))))

(defn mover-step [m]
  (assoc m :loc (add (:loc m) (:vel m))))

(defn get-older [m]
  (update-in m [:age] inc))

(defn mover [opts]
  (merge {:loc    v0
          :vel    v0
          :mass   1
          :show   show-default
          :upd    (comp get-older mover-step apply-forces)
          :action (fn [x y] y)
          :age 0}
         opts))

(defn env-step [{:keys [elements forces actions] :as env}]
  (let [els (reduce (fn [es [i e]]
                      (map-indexed
                        (fn [ii ee] (if (not= i ii) ((:action e) e ee) ee))
                        es))
                    elements
                    (map-indexed vector elements))
        els (map #(apply-forces % forces) els)
        els (map #((:upd %) %) els)
        els (map #(reduce (fn [a f] (f env a)) % actions) els)]
    (assoc env :elements els)))

(defn env [opts]
  (merge {:forces        []
          :elements      []
          :actions       []
          :upd           env-step
          :show-self     (fn [e] (background 0))
          :show-elements (fn [e] (mapv #((:show %) %) (:elements e)))
          :show          (fn [e] ((:show-self e) e) ((:show-elements e) e))
          :bounds        [500 500]}
         opts))


(do
  "test1"

  (defn show-mover [rad {[x y] :loc}]
    (let [i (/ rad 25)]
      (doseq [n (range 25)]
      (do
        (fill 255 255 255 5)
        (ellipse x y (* n i) (* n i))))))

  (defn show-mover2 [{a :age [x y] :loc}]
    (let [rad (* 60 (noise (/ a 100)))
          c (* 150 (noise (/ (+ a 4000) 100)))]
      (fill (+ 100 c) 0 0)
      (ellipse x y rad rad)))

  (def G 2)

  (defn gravitation [x y]
    (let [dist (sub (:loc y) (:loc x))
          ma (magn dist)
          fma (/ (* G (:mass x) (:mass y)) (* ma ma))
          f (-> dist normalize (mult fma) (lim 0.1))]
      (update-in y [:vel] #(add % f))))


  (defn rand-mover [velrand locrand]
    (mover {:vel [(rand velrand) (rand velrand)]
            :loc [(rand locrand) (rand locrand)]
            :show show-mover2 #_(partial show-mover (rand-nth (range 25 75)))
            :mass (rand-nth (range 0.5 2 0.25))
            :action gravitation
            :age (int (* 1000 (rand)))}))

  (defn unescapable [{:keys [bounds] :as env} {[x y :as loc] :loc :as m}]
    (let [[w h] bounds]
      (assoc m :loc
               (cond
                 (> x w) (setx loc 0)
                 (> 0 x) (setx loc w)
                 (> y h) (sety loc 0)
                 (> 0 y) (sety loc h)
                 :else loc))))

  (defn friction [_ {v :vel :as m}]
    (let [fcoef 0.001
          vmag (magn v)
          fmag (* fcoef vmag vmag)
          fric (mult (normalize v) (- fmag))]
      (assoc m :vel (add v fric))))

  (defn setup []
    (no-stroke)
    (stroke-weight 10)
    (background 0)
    (env
      {:elements (mapv (fn [_] (rand-mover 1 500)) (range 100))
       :actions  [unescapable friction]
       :forces [[0 0.0001]]
       :bounds [1500 1000]}))

  (defn update [e] ((:upd e) e))

  (defn draw [e]

    ((:show e) e))

  (defsketch ex
               :size [1500 1000]
               :setup setup
               :draw draw
               :update update
               ; :mouse-moved mouse-moved
               :middleware [m/fun-mode])
  )

