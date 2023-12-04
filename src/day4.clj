(ns day4
  (:use oskarkv.utils
        com.rpl.specter
        core)
  (:require [clojure.string :as str]
            [clojure.math :as m]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(defn parse-line [line]
  (->> (str/split line #":|\|")
    (map #(re-seq #"\d+" %))))

(defn card->matches [input]
  (->> (map parse-line input)
    (reduce (fn [m [[c] a b]]
              (conj m [(parse-long c) (count (intersection a b))]))
            {})))

(defn solve-4-1 [input]
  (->> input
    card->matches
    (map second)
    (map #(m/pow 2 (dec %)))
    (filter #(>= % 1))
    sum))

(defn solve-4-2 [input]
  (let [m (card->matches input)
        num-wins (memoize (fn num-wins [n]
                            (if-lets [wins (m n)
                                      cards (take wins (iterate inc (inc n)))]
                              (inc (sum (map num-wins cards)))
                              1)))]
    (sum (map num-wins (keys m)))))
