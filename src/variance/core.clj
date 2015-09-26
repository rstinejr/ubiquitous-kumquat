(ns variance.core
  (:gen-class))

(defn- var-from-sums
  [ss s cnt]
  (when (<= cnt 0)
    (throw (ex-info "Most have one or more samples to compute variance." {:causes #{:div-by-zero :bad-args}})))
  (double (/ (- ss (/ (* s s) cnt)) cnt)))

(defn calculate-variance
  [sample]
  (let [sums  (loop [[n & the-rest] sample
                     tallies        {:sumsq 0 :sum 0}]
                (if (nil? n)
                  tallies
                  (recur the-rest {:sumsq (+ (:sumsq tallies) (* n n)) 
                                   :sum   (+ (:sum   tallies) n)})))]
    (var-from-sums (:sumsq sums) (:sum sums) (count sample))))

(defn calculate-variance2
  [sample]
  (let [tallies (reduce 
                  (fn [tallies n] 
                    {:sumsq (+ (:sumsq tallies) (* n n)) 
                     :sum   (+ (:sum   tallies) n)})
                  {:sumsq 0 :sum 0} 
                  sample)]
    (var-from-sums (:sumsq tallies) (:sum tallies) (count sample))))

(defn -main
  [& args]
  (let [sample (concat (map #(Integer. %) args))]
    (println "sample:" sample)
    (println (str "variance from loop:       " (calculate-variance sample)))
    (println (str "variance from map/reduce: " (calculate-variance2 sample)))))
