(ns noc.ex1-3
  (:require [quil.core :refer :all]
            [quil.middleware :as m]))

(def p (atom [100 100]))

(defn rand-help []
  (rand-nth [-2 -1 1 2]))

(defn wrand [wm]
  (let [[ks vs] ((juxt keys vals) wm)
        vs (reductions + 0 vs)
        wm (apply sorted-map (interleave (butlast vs) ks))
        picked (* (rand) (last vs))]
    (val (last (take-while (fn [[k v]] (< k picked)) wm)))))

(def dirs {:t [0 1] :b [0 -1] :r [1 0] :l [-1 0]
           :tl [-1 1] :tr [1 1] :bl [-1 -1] :br [1 -1] :i [0 0]})

(defn dir-wrand [wm]
  (wrand (into {} (map (fn [[k v]] [(dirs k) v]) wm))))

(def unidirand
  (apply hash-map
         (interleave
           [:t :b :r :l :tl :tr :bl :br :i]
           (repeat 1))))

(defn p-step [mx my delt]
  (let [[x y] @p
        diffx (- x mx)
        diffy (- y my)

        mrelpos (condp = [(compare 0 diffx) (compare 0 diffy)]
                  [0 0] :i
                  [0 1] :t
                  [0 -1] :b
                  [1 0] :r
                  [-1 0] :l
                  [-1 1] :tl
                  [1 1] :tr
                  [-1 -1] :bl
                  [1 -1] :br)
        [dx dy] (dir-wrand (update-in unidirand [mrelpos] * 5))]

    (reset! p [(+ x dx) (+ y dy)])))

random-gaussian

#_(p-step 0 110 3)

(defn draw []
  (apply point (p-step (mouse-x) (mouse-y) 3)))


; 'setup' is a cousin of 'draw' function
; setup initialises sketch and it is called only once
; before draw called for the first time
(defn setup []
  ; draw will be called 60 times per second
  (frame-rate 100)
  (stroke 100)
  (stroke-weight 1)
  (smooth)
  (reset! p [(/ (width) 2) (/ (height) 2)])
  ; set background to white colour only in the setup
  ; otherwise each invocation of 'draw' would clear sketch completely
  (background 255))

(defsketch noc
           :title "You spin my circle right round"
           :size [500 500]
           ;:update update
           :setup setup
           :draw draw)
