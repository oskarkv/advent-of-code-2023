(ns day1
  (:use oskarkv.utils
        com.rpl.specter
        core)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(defn find-number-fn [also-words reverse]
  (fn [string]
    (let [nums (take 9 (rest N))
          numerals (cond->> (map #(pp/cl-format nil "~R" %) nums)
                     reverse (map str/reverse))
          pattern (if also-words
                    (re-pattern (str (interpose-str "|" numerals) "|\\d"))
                    #"\d")]
      (->>$ (re-find pattern (cond-> string reverse str/reverse))
        (or ((zipmap numerals (map str nums)) $) $)
        parse-long))))

(defn solve-1 [input also-words]
  (->> (str/split input #"\n")
    (map (apply juxt (map #(find-number-fn also-words %) [false true])))
    (map (comp parse-int #(apply str %)))
    sum))
