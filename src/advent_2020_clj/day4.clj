(ns advent-2020-clj.day4
  (:require
   [clojure.string :as str]
   [clojure.set :as st]))

(def input (slurp "src/advent_2020_clj/day4Input.txt"))

(def necessary-keys #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn passport->map [ppt]
  (as-> ppt $
    (str/split $ #"[\s\n]")
    (map #(str/split % #":") $)
    (reduce (fn [acc [k v]]
              (assoc acc k v))
            {}
            $)
    ))

(defn right-keys? [ppt-map]
  (as-> ppt-map $
    (keys $)
    (set $)
    (st/subset? necessary-keys $)))

(defn all-digits? [s]
  (re-matches #"[0-9]+" s))

(defn between? [s min max]
  (let [i (Integer/parseInt s)]
    (and (<= min i)
         (>= max i))))

(defn valid-byr? [ppt-map]
  (let [byr (get ppt-map "byr")]
    (and (= 4 (count byr))
         (between? byr 1920 2020))))

(defn valid-iyr? [ppt-map]
  (let [iyr (get ppt-map "iyr")]
    (and (= 4 (count iyr))
         (between? iyr 2010 2020))))

(defn valid-eyr? [ppt-map]
  (let [eyr (get ppt-map "eyr")]
    (and (= 4 (count eyr))
         (between? eyr 2020 2030))))

(defn valid-hgt? [ppt-map]
  (let [hgt (get ppt-map "hgt")]
    (if-let [cm-height (re-matches #"[0-9]+cm" hgt)]
      (let [height (Integer/parseInt (re-find #"[0-9]+" cm-height))]
        (and (<= height 193)
             (>= height 150)))
      (if-let [in-height (re-matches #"[0-9]+in" hgt)]
        (let [height (Integer/parseInt (re-find #"[0-9]+" in-height))]
          (and (<= height 76)
               (>= height 59)))
        false))))

(defn valid-hcl? [ppt-map]
  (let [hcl (get ppt-map "hcl")]
    (re-matches #"#[0-9a-f]{6}" hcl)))

(defn valid-ecl? [ppt-map]
  (let [ecl (get ppt-map "ecl")]
    (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)))

(defn valid-pid? [ppt-map]
  (let [pid (get ppt-map "pid")]
    (re-matches #"[0-9]{9}" pid)))

(defn day4-ans1 [ipt]
  (as-> ipt $
    (str/split $ #"\n\n")
    (map passport->map $)
    (filter right-keys? $)
    (count $)))

(defn day4-ans2 [ipt]
  (as-> ipt $
    (str/split $ #"\n\n")
    (map passport->map $)
    (filter (every-pred right-keys? valid-byr? valid-iyr? valid-eyr? valid-hgt? valid-hcl? valid-ecl? valid-pid?) $)
    (count $)))

(day4-ans1 input)

(day4-ans2 input)
