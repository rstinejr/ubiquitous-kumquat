(ns variance.core
  (:gen-class))

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
    (double (- (/ ss cnt) (/ (/ (* s s) cnt) cnt)))))

(defn calculate-variance2
  [sample]
  (let [[ss s] (reduce 
                     (fn [pair1 pair2] 
                       [ (+ (pair1 0) (pair2 0)) (+ (pair1 1) (pair2 1))])
                     [0 0] 
                     [[4 2 ] [9 3] [1 1] [0 0]])]
        (println (str "sum sqrs: " ss ", sum: " s ", count: " (count sample)))))

(defn -main
  [& args]
  (let [sample (concat (map #(Integer. %) args))]
    (println "sample:" sample)
    (calculate-variance2 sample)
    (println (str "variance: " (calculate-variance sample)))))
