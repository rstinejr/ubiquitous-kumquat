(ns variance.core
  (:gen-class))

(defn- var-from-sums
  [ss s cnt]
  (double (- (/ ss cnt) (/ (/ (* s s) cnt) cnt))))

(defn calculate-variance
  [sample]
  (let [[ss s cnt] (loop [[n & the-rest] sample
                         sumsq          0
                         sum            0
                         cnt            0]
                     (if n
                       (recur the-rest (+ sumsq (* n n)) (+ sum n) (inc cnt))
                       [sumsq sum cnt]))]
    (when (= 0 cnt)
      (throw (ex-info "Most have one or more samples to compute variance." {:causes #{:div-by-zero :bad-args}})))
    (var-from-sums ss s cnt)))

(defn calculate-variance2
  [sample]
  (let [[ss s cnt] (reduce 
                     (fn [trip1 trip2] 
                       [ (+ (trip1 0) (trip2 0)) (+ (trip1 1) (trip2 1)) (inc (trip1 2))])
                     [0 0 0] 
                     (map (fn [n] [(* n n) n nil]) sample))] 
    ;(println (str "ss: " ss ", s: " s ", cnt: " cnt))
    (var-from-sums ss s cnt)))

(defn -main
  [& args]
  (let [sample (concat (map #(Integer. %) args))]
    (println "sample:" sample)
    (println (str "variance from loop:       " (calculate-variance sample)))
    (println (str "variance from map/reduce: " (calculate-variance2 sample)))))
