(ns variance.core
  (:gen-class))

(defn- var-from-sums
  [ss s cnt]
  (when (<= cnt 0)
    (throw (ex-info "Most have one or more samples to compute variance." {:causes #{:div-by-zero :bad-args}})))
  (double (/ (- ss (/ (* s s) cnt)) cnt)))

(defn calculate-variance
  [sample]
  (let [tallies (loop [[n & the-rest] sample
                       m              {:sumsq 0 :sum 0 :cnt 0}]
                     (if n
                       (recur the-rest {:sumsq (+   (:sumsq m) (* n n)) 
                                        :sum   (+   (:sum   m) n) 
                                        :cnt   (inc (:cnt   m))})
                       m))]
    (var-from-sums (:sumsq tallies) (:sum tallies) (:cnt tallies))))

(defn calculate-variance2
  [sample]
  (let [tallies (reduce 
                  (fn [m1 m2] 
                    {:sumsq (+   (:sumsq m1) (:sumsq m2)) 
                     :sum   (+   (:sum   m1) (:sum   m2)) 
                     :cnt   (inc (:cnt   m1))})
                  {:sumsq 0 :sum 0 :cnt 0} 
                  (map (fn [n] {:sumsq (* n n) :sum n}) sample))] 
    (var-from-sums (:sumsq tallies) (:sum tallies) (:cnt tallies))))

(defn -main
  [& args]
  (let [sample (concat (map #(Integer. %) args))]
    (println "sample:" sample)
    (println (str "variance from loop:       " (calculate-variance sample)))
    (println (str "variance from map/reduce: " (calculate-variance2 sample)))))
