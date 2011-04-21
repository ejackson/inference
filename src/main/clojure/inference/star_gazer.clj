(ns inference.star-gazer
  (:use [incanter.core    :as i]
	[incanter.charts :as c]))

;; A starfield is an sorted set of numbers between 0 and 1 representing
;; the x position of the star.
(defn star-field [n]
     (apply sorted-set (take n (repeatedly rand))))

;; Return all the stars to the left of randomly placed wall.
(defn obscure-field [field]
  (let [wall-pos (rand)]
    {:wall  wall-pos
     :field (take-while #(<= % wall-pos) field)}))

;; Measure the gap between the last star and the wall.  Account for cases
;; where wall < field
(defn gap [field]
  (if-let [last-star (-> field :field last)]
    (- (:wall field) last-star)))

;; Perform the experiment.  Filter is for the nils when wall < last_star
(defn gap-stats [num-stars num-samples]
  (filter identity
	  (take num-samples
		(repeatedly
		 (fn []
		   (gap (obscure-field (star-field num-stars))))))))

;; Show me the money, honey.
(i/view (c/histogram (gap-stats 100 1000)
		     :nbins 500
		     :density true
		     :title "Final Gap Length"))

