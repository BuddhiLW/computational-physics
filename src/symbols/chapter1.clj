(ns symbols.chapter1
  (:require
   [emmy.env :as e :refer :all]
   [emmy.generic :as g]))

(defn L-free-particle
  "The lagrangian of a free particle of mass m. The Lagrangian
  returned is a function of the local tuple. Since the particle
  is free, there is no potential energy, so the Lagrangian is
  just the kinetic energy."
  [mass]
  (fn [[_ _ v]]
    (* (/ 1 2) mass (g/square v))))

(comment
 ((L-free-particle 1) [0 0 [1 1 1]])

 (g/square [1 2 3])

 (->TeX
  (g/square (g/sin (+ 'a 3))))
;; => "{\\sin}^{2}\\left(a + 3\\right)"

 (->infix
  (g/square (g/sin (+ 'a 3)))))
;; => "sin²(a + 3)"

(def q (up (literal-function 'x)
           (literal-function 'y)
           (literal-function 'z)))
(q 't)
;; => (up (x t) (y t) (z t))

((D q) 't)
;; => (up ((D x) t) ((D y) t) ((D z) t))

((Gamma q) 't)
;; => (up t (up (x t) (y t) (z t)) (up ((D x) t) ((D y) t) ((D z) t)))

;; L◦Ⲅ
;; (compose 'L 'Gamma) .. pseudo-code

(->TeX
 (->infix
  (simplify
   ((compose (L-free-particle 'm) (Gamma q)) 't))))
;; => "1/2 m (Dx(t))² + 1/2 m (Dy(t))² + 1/2 m (Dz(t))²"

(comment
 (->infix
  ;; => "up(4 t + 7, 3 t + 5, 2 t + 1)")

  (let [x (fn [t] (+ (* 4 t) 7))
        y (fn [t] (+ (* 3 t) 5))
        z (fn [t] (+ (* 2 t) 1))
        q (up x y z)
        path (q 't)]
    path)))
  ;; => (up (+ (* 4 t) 7) (+ (* 3 t) 5) (+ (* 2 t) 1)))


(let [x (fn [t] (+ (* 4 t) 7))
      y (fn [t] (+ (* 3 t) 5))
      z (fn [t] (+ (* 2 t) 1))
      q (up x y z)
      path (fn [t] (q t))
      m 3
      t1 0.0
      t2 10.0]
 (emmy.mechanics.lagrange/Lagrangian-action
  (L-free-particle m)
  path t1 t2))
;; => 435.0
