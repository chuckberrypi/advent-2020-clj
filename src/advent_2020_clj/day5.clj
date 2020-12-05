(ns advent-2020-clj.day5
  (:require
   [clojure.set :as st]
   [clojure.string :as str]))

(def input (slurp "inputs/day5.txt"))

(defn split-axes [s]
  [(re-find #"[FB]{7}" s) (re-find #"[LR]{3}" s)])

(defn range-split [{:keys [min max]}]
  (let [diff (- max min)
        step (int (/ diff 2))
        low-max (+ min step)
        high-min (inc low-max)]
    (if (= min max)
      (let [r {:min min :max max}]
        {:range-low r
         :range-high r})
      {:range-low {:min min
                   :max low-max}
       :range-high {:min high-min
                    :max max}})))

(defn translate-str [s low high low-chr]
  (loop [[f & r :as sq] (seq s)
         range {:min low :max high}
         ]
    (let [{:keys [range-low range-high] :as ranges} (range-split range)
          {:keys [min max] :as keep-range} (if (= f low-chr) range-low range-high)
          ]
      (if (or (= min max)
              (not f))
        min
        (recur r keep-range))
      )
    ))

(defn pass->seat [s]
  (let [[row-str col-str] (split-axes s)
        row (translate-str row-str 0 127 \F)
        col (translate-str col-str 0 7 \L)]
    {:row row
     :col col}))

(defn seat->id [{:keys [row col]}]
  (+ col (* 8 row))
  )

(defn str->id [s]
  (-> s pass->seat seat->id))

(defn above-and-below? [n st]
  (and (st (inc n))
       (st (dec n))))

(defn day5-ans1 [inpt]
  (->> inpt
       (str/split-lines)
       (map pass->seat)
       (map seat->id)
       (apply max)))

(defn day5-ans2 [inpt]
  (let [passes (str/split-lines inpt)
        seats (map pass->seat passes)
        ids (apply hash-set (map seat->id seats))
        min-possible-id (str->id "FFFFFFFLLL")
        max-possible-id (str->id "BBBBBBBRRR")
        possible-ids (apply hash-set (range min-possible-id (inc max-possible-id)))
        open-ids (st/difference possible-ids ids)
        ]
    (first (filter #(above-and-below? % ids) open-ids))))


