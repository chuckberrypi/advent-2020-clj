(ns advent-2020-clj.day6
  (:require
   [clojure.string :as str]
   [clojure.set :as s]))

(def input (slurp "inputs/day6.txt"))

(defn group->set [group-string]
  (->> group-string
       seq
       (remove #(= % \newline))
       (into #{})
       ))

(defn group->set2 [group-string]
  (->> (str/split-lines group-string)
       (map #(into #{} (seq %)))
       (apply s/intersection))
 )

(defn day6-ans1 [inpt]
  (as-> inpt $
    (str/split $ #"\n\n")
    (map group->set $)
    (map count $)
    (reduce + $)
    ))

(defn day6-ans2 [inpt]
  (->> (str/split inpt #"\n\n")
       (map group->set2)
       (map count)
       (reduce +)))

(comment

  (as-> input $
    (str/split $ #"\n\n")
    (second $)
    (group->set2 $)

      )

  (day6-ans1 input)

  (day6-ans2 input)

  )


