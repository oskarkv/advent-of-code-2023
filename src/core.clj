(ns core
  (:use oskarkv.utils
        com.rpl.specter)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(defn mapp [f coll]
  (map #(map f %) coll))

(def N (iterate inc 0))

(defn read-raw-input [n]
  (str/trim (slurp (str "input/" n ".txt"))))

(defn read-input [n]
  (str/split-lines (read-raw-input n)))
