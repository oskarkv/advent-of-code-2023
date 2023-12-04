(ns day3
  (:use oskarkv.utils
        com.rpl.specter
        core)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))


(defn adjacent [x y]
  (for [dx [-1 0 1] dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn parts [input]
  (->> (zip N input)
    (map (fn [[x line]]
           (->>$ (zip N line)
             (remove (fn [[y c]] ((set "0123456789.") c)))
             (map (fn [[y c]] (zipmap (adjacent x y) (repeat (gensym c)))))
             (apply merge))))
    (apply merge)))

(defn line-numbers [line]
  (loop [i 0 line line result []]
    (if-not (seq line)
      result
      (if-lets [num (re-find #"^\d+" line)
                n (count num)]
        (recur (+ i n)
               (drop-str n line)
               (conj result [(parse-long num) (set (take n (iterate inc i)))]))
        (recur (inc i) (rest-str line) result)))))

(defn numbers [input]
  (->> (zip N input)
    (map (fn [[x line]]
           (->> (line-numbers line)
             (transform [ALL 1 ALL] #(vector x %)))))
    (apply concat)))

(defn solve-3-1 [input]
  (let [ps (parts input)]
    (->> (zip N input)
      (map (fn [[x line]]
             (->> (line-numbers line)
               (filter (fn [[num ys]] (some ps (map #(vector x %) ys))))
               (map first)
               sum)))
      sum)))

(defn solve-3-2 [input]
  (let [ps (parts input)
        ns (numbers input)]
    (->> (transform [ALL 1 ALL] ps ns)
      (map (fn [[num syms]] (into {} (for [s syms] [s [num]]))))
      (apply merge-with #(conj % (first %2)))
      (select [ALL #(= 2 (count (second %))) #(= \* (first (str (first %))))])
      (map (comp product second))
      sum)))
