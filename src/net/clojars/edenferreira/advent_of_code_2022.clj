(ns net.clojars.edenferreira.advent-of-code-2022
   (:require [clojure.string :as string]
             [clojure.set :as set]))



(defmacro defclifn [n in-name-vec & body]
  `(defn ~n [~'& [~'manual-in]]
     (let [~(first in-name-vec)
           (or ~'manual-in
               (line-seq (java.io.BufferedReader. *in*)))
           res# (do ~@body)]
       (if ~'manual-in
         res#
         (println res#)))))

(defclifn calories [in]
   (transduce
     (comp (partition-by empty?)
           (remove (comp empty? first))
           (map (partial map parse-long))
           (map (partial reduce +)))
     (fn
       ([val] val)
       ([cmax val]
        (max cmax val)))
     0
     in))

(defclifn calories2 [in]
  (->> in
       (transduce
         (comp (partition-by empty?)
               (remove (comp empty? first))
               (map (partial map parse-long))
               (map (partial reduce +)))
         (fn
           ([val] val)
           ([maxes val]
            (take 3 (sort #(compare %2 %1) (conj maxes val)))))
         [0])
       (reduce +)))



(defclifn jokenpo [in]
  (let [round-outcome (fn [a b]
                        (if (= (:name a) (:name b))
                          :draw
                          (get a (:name b))))
        rock {:name :rock
              :score 1
              :scissor :win
              :paper :lose}
        paper {:name :paper
               :score 2
               :rock :win
               :scissor :lose}
        scissor {:name :scissor
                 :score 3
                 :paper :win
                 :rock :lose}
        jokenpo-data {"A" rock
                      "X" rock
                      "B" paper
                      "Y" paper
                      "C" scissor
                      "Z" scissor}
        round-outcome-points {:win 6
                              :draw 3
                              :lose 0}]
    (transduce
      (comp (map #(string/split % #" "))
            (map reverse)
            (map (partial map #(get jokenpo-data %)))
            (map (juxt (comp :score first)
                       (comp round-outcome-points (partial apply round-outcome))))
            (map (partial apply +)))
      +
      0
      in)))

(comment
  (prn
   (jokenpo
    (string/split-lines
     "A Y
B X
C Z")))

  (calories2
    (string/split-lines
      "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"))

  (calories
   (string/split-lines
    "3264
4043
2537
3319
2485
3218
5611
1753
7232
3265
1751
2233

10589
5121
11465
9307
1347
9392
1203")))
