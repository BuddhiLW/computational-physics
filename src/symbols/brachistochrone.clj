(ns symbols.brachistochrone
  (:require
   [emmy.env :as e :refer :all]
   [emmy.generic :as g]
   [emmy.mechanics.lagrange :as lagrange]))

;; Define gravity
(def g 9.81)

;; Define the Lagrangian of a particle under gravity
(defn L-brachistochrone
  "The Lagrangian for a particle moving under gravity in 2D."
  [mass]
  (fn [[_ [x y] [vx vy]]]
    (- (* 1/2 mass (g/+ (g/square vx) (g/square vy)))  ;; Kinetic Energy
       (* mass g y)))) ;; Potential Energy (V = mgy)

;; Parametric path representation (x, y) as functions of t
(def q (up (literal-function 'x)
           (literal-function 'y)))

;; Evaluate q at time t
(q 't)
;; => (up (x t) (y t))

;; Compute the velocity (first derivative of q)
((D q) 't)
;; => (up ((D x) t) ((D y) t))



;; Compute the Euler-Lagrange equations
(def EL-eqs
   (->infix
    (simplify
     ((compose (L-brachistochrone 'm) (Gamma q)) 't))))

;; Convert to LaTeX for visualization
(->TeX EL-eqs)
;; => "1/2 m (Dx(t))² + 1/2 m (Dy(t))² - 9.81 m y(t)")

;; This should give the equations of motion for the brachistochrone problem


(let [state (up (literal-function 'x)
                (literal-function 'y))
      L (L-brachistochrone 'm)
      equations (((lagrange/Lagrange-equations L) state) 't)]
  equations
  #_(->TeX
     (->infix
      (simplify equations))))
;; => (down (* 1/2 m 2 (((expt D 2) x) t)) (- (* 1/2 m 2 (((expt D 2) y) t)) (* -1 m 9.81)))
;; => "down(m D²x(t), m D²y(t) + 9.81 m)"


;; Define the Lagrangian for the variational problem
(defn L-brachistochrone-variational
  "Lagrangian for the brachistochrone time integral."
  [y0]
  (fn [[x y yp]]  ;; Ensure proper tuple handling
    (/ (g/sqrt (g/+ 1 (g/square yp)))
       (g/sqrt (g/* 2 g (g/- y0 y))))))

;; Define the function y(x) as a literal function
(def state (up (literal-function 'y)))

;; Check if the Lagrangian evaluates correctly
(let [y0 'y0
      L (L-brachistochrone-variational y0)]
  (L '[x y yp]))
;; => (/ (sqrt (+ 1 (expt yp 2))) (sqrt (* 19.62 (- y0 y))))

;; Define the function y(x) as a literal function
(def state (literal-function 'y))  ;; Define a single function

;; Compute the Euler-Lagrange equation for y(x)
(let [y0 'y0
      L (L-brachistochrone-variational y0)]
  (->TeX
   (->infix
    (simplify
     (((lagrange/Lagrange-equations L) state) 'x)))))
;; => "(39.24 y0 D²y(x) - 19.62 (Dy(x))² - 39.24 D²y(x) y(x) + -19.62) / (39.24 y0 (Dy(x))² sqrt(19.62 y0 (Dy(x))² - 19.62 (Dy(x))² y(x) + 19.62 y0 - 19.62 y(x)) - 39.24 (Dy(x))² y(x) sqrt(19.62 y0 (Dy(x))² - 19.62 (Dy(x))² y(x) + 19.62 y0 - 19.62 y(x)) + 39.24 y0 sqrt(19.62 y0 (Dy(x))² - 19.62 (Dy(x))² y(x) + 19.62 y0 - 19.62 y(x)) - 39.24 y(x) sqrt(19.62 y0 (Dy(x))² - 19.62 (Dy(x))² y(x) + 19.62 y0 - 19.62 y(x)))"

;; Define gravity symbolically
(def g-symbol 'g)

;; Define the Lagrangian for a particle under gravity in 2D
(defn L-brachistochrone
  "The Lagrangian for a particle moving under gravity in 2D."
  [mass]
  (fn [[_ [x y] [vx vy]]]
    (- (* 1/2 mass (g/+ (g/square vx) (g/square vy)))  ;; Kinetic Energy
       (* mass g-symbol y)))) ;; Potential Energy (V = mg y)

;; Define the Lagrangian for the variational problem
(defn L-brachistochrone-variational
  "Lagrangian for the brachistochrone time integral."
  [y0]
  (fn [[x y yp]]  ;; Ensure proper tuple handling
    (/ (g/sqrt (g/+ 1 (g/square yp)))
       (g/sqrt (g/* 2 g-symbol (g/- y0 y))))))

;; Define the function y(x) as a literal function
(def state (literal-function 'y))  ;; Define a single function
;; Compute the Euler-Lagrange equation for y(x)
(let [y0 'y0
      L (L-brachistochrone-variational y0)]
  (->TeX
   (->infix
     (simplify
      (((lagrange/Lagrange-equations L) state) 'x)))))
;; => "(2 y0 D²y(x) - (Dy(x))² - 2 D²y(x) y(x) + -1) / (2 y0 (Dy(x))² sqrt(2 g y0 (Dy(x))² - 2 g (Dy(x))² y(x) + 2 g y0 - 2 g y(x)) - 2 (Dy(x))² y(x) sqrt(2 g y0 (Dy(x))² - 2 g (Dy(x))² y(x) + 2 g y0 - 2 g y(x)) + 2 y0 sqrt(2 g y0 (Dy(x))² - 2 g (Dy(x))² y(x) + 2 g y0 - 2 g y(x)) - 2 y(x) sqrt(2 g y0 (Dy(x))² - 2 g (Dy(x))² y(x) + 2 g y0 - 2 g y(x)))"
