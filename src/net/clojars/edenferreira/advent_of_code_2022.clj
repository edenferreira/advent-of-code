(ns net.clojars.edenferreira.advent-of-code-2022
   (:require [clojure.string :as string]))

(defn calories* [in]
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

(defn calories2* [in]
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

(defn calories [& _]
  (println (calories* (line-seq (java.io.BufferedReader. *in*)))))

(defn calories2 [& _]
  (println (calories2* (line-seq (java.io.BufferedReader. *in*)))))

(comment
  (calories2*
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

  (calories*
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

  (calories*
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
1203"))
)
