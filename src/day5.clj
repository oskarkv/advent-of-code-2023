(ns day5
  (:use oskarkv.utils
        com.rpl.specter
        core)
  (:require [clojure.string :as str]
            [clojure.math :as m]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(defn parse-map [input]
  (map (fn [line] (map parse-long (str/split line #" ")))
       (rest input)))

(defn parse-input [input]
  (let [seeds (map parse-long (re-seq #"\d+" (first input)))]
    [seeds (->> (drop 2 input)
             (partition-by #(= "" %))
             (take-nth 2)
             (map parse-map))]))

(defn destination-range [[dest source len] [start range]]
  (let [new-start (max start source)
        end (min (+ source len) (+ start range))
        new-range (- end new-start)]
    (when (pos? new-range)
      [(+ new-start (- dest source)) new-range])))

(defn find-next-range [m input]
  (keep #(destination-range % input) m))

(defn find-next-fn [m]
  (fn [ranges]
    (mapcat #(find-next-range m %) ranges)))

(defn solve-5 [input first-part]
  (let [[seeds maps] (parse-input input)
        ranges (if first-part
                 (map #(vector % 1) seeds)
                 (partition 2 seeds))]
    (->> ((apply comp (reverse (map find-next-fn maps))) ranges)
      (map first)
      (apply min))))
