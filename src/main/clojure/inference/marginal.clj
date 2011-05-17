(ns inference.marginal
  (:use [incanter.core   :as i]
	[incanter.charts :as c]))

(defn trapz
  "Use the trapezoidal rule to integrate a function between a and b using n intervals"
  [f a b n]
  (let [d (/ (- b a) n)
        r (range a b d)
        v (map f r)]
    (* d (+ (apply + (rest v))
            (/ (first v) 2)
            (/ (f b) 2)))))

(defn dnorm
  "Density of the normal distribution at a x"
  [mu sigma x]
  (/ (Math/exp (/ (Math/pow (- x mu) 2) (* -2 sigma sigma)))
     (Math/sqrt (* 2 Math/PI sigma sigma))))

(defn rnorm
  "Draw from a random distribution using a cunning rejection rule"
  [mu sigma]
  (let [u (rand)
        v (* 1.7156 (- (rand) 0.5))
        x (- u 0.449871)
        y (+ (Math/abs v) 0.386595)
        q (+ (* x x)
             (* y (- (* 0.19600 y)
                     (* 0.25472 x))))]
    (if (not (and (> q 0.27597)
                  (or (> q 0.27846)
                      (> (* v v)
                         (* -4 (Math/log u) (* u u))))))
      (+ mu (* sigma (/ v u)))
      (rnorm mu sigma))))

;; Here lurk numerical precision issues if you are not careful.  It would be better
;; to put everything into log and go from there.
(defn likelihood [mu sigma xs]
  (apply * (map (partial dnorm mu sigma) xs)))

(defn sample-normals [n mu sigma]
  (repeatedly n (partial rnorm mu sigma)))

(defn marginal-sigma-likelihood [xs sigma]
  (trapz
   (fn [x] (likelihood x sigma xs))
   -50 50 100))

(defn marginal-sigma-posterior [xs sigma]
  (* (trapz
      (fn [x] (likelihood x sigma xs))
      -50 50 100)
     (/ 1 sigma)))

(defn sample-mean [xs]
  (/ (apply + xs) (count xs)))

(defn sample-stdev [xs biased?]
  (let [mu-bar (sample-mean xs)
        n (if biased? (count xs) (dec (count xs)))]
    (Math/sqrt (/ (apply + (map
                            (fn [x]
                              (Math/pow (- x mu-bar) 2))
                            xs))
                  n))))

(comment
  ;; Have a look at these purported normals
  (i/view (c/histogram (sample-normals 10000 0 1)
                       :nbins 50
                       :density true
                       :title "Sampled Distribution"))

  ;; Now do the experiment
  (def my-xs (sample-normals 10 0 2))
  (prn (str "Sample Mean: " (sample-mean my-xs)))
  (prn (str "Sample Std Max-L   : " (sample-stdev my-xs true)))
  (prn (str "Sample Std Unbiased: " (sample-stdev my-xs false)))

  (i/view (doto (c/function-plot (partial marginal-sigma-likelihood my-xs) 0.1 5.0)
            (c/add-function (partial marginal-sigma-posterior my-xs) 0.1 5.0)))
  )
