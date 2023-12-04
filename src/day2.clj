(ns day2
  (:use oskarkv.utils
        com.rpl.specter
        core)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(defn parse-line [line]
  (let [id (parse-long (re-find #"\d+" line))
        sets (rest (str/split line #": |; "))]
    [id (map #(->> (str/split % #", | ")
                reverse
                (apply hash-map)
                (fmap parse-long)
                walk/keywordize-keys
                (merge {:red 0 :green 0 :blue 0}))
             sets)]))

(defn possible? [{:keys [red green blue]}]
  (every-in? (map <= [red green blue] [12 13 14])))

(defn solve-2-1 [input]
  (->> (map parse-line input)
    (filter (fn [[id sets]] (every? possible? sets)))
    (map first)
    sum))

(defn power [maps]
  (-> (apply merge-with max maps) vals product))

(defn solve-2-2 [input]
  (->> (map parse-line input)
    (map second)
    (map power)
    sum))
