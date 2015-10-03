(ns noc.utils.pv2)

(def v0 [0 0])

(def getx first)
(def gety second)
(defn setx [[_ y] x] [x y])
(defn sety [[x _] y] [x y])
(defn updx [[x y] z & args] [(apply z x args) y])
(defn updy [[x y] z & args] [x (apply z y args)])

(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn sub [[a b] [c d]]
  [(- a c) (- b d)])

(defn mult [[a b] c]
  [(* a c) (* b c)])

(defn div [v c] (mult v (/ c)))

(defn magn [[a b]]
  (Math/sqrt (+ (* a a) (* b b))))

(defn normalize [v]
  (div v (magn v)))

(defn magn! [v m]
  (mult (normalize v) m))

(defn lim [v l]
  (if (> (magn v) l)
    (magn! v l)
    v))