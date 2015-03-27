(ns om-svg-tut.sudoku
  (:require [clojure.string :as string]
            [goog.string :as gstring]
            [goog.string.format]))

;; ideas from http://norvig.com/sudoku.html
;; ported to clojurescript
(enable-console-print!)
;; squares units peers

(defn format
  "Formats a string using goog.string.format."
  [fmt & args]
  (apply gstring/format fmt args))

(def digits "123456789")
(def rows "ABCDEFHGI")
(def cols digits)
(defn cross [A B]
  (for [a A b B] (str a b)))
(def squares (cross rows cols))
(def unitlist (concat (for [c cols] (cross rows c))
                      (for [r rows] (cross r cols))
                      (for [rs ["ABC" "DEF" "GHI"] cs ["123" "456" "789"]]
                        (cross rs cs))))
(def units (into {}
                 (for [s squares]
                   [s (for [u unitlist :when (some #{s} u)] u)])))
(def peers
  (into {}
        (for [s squares]
          [s (-> (reduce into #{} (units s)) (disj s))])))

(def b1
  [4 0 0  0 0 0  8 0 5
   0 3 0  0 0 0  0 0 0
   0 0 0  7 0 0  0 0 0

   0 2 0  0 0 0  0 6 0
   0 0 0  0 8 0  4 0 0
   0 0 0  0 1 0  0 0 0

   0 0 0  6 0 3  0 7 0
   5 0 0  2 0 0  0 0 0
   1 0 4  0 0 0  0 0 0])

(def b2
  [0 0 3  0 2 0  6 0 0
   9 0 0  3 0 5  0 0 1
   0 0 1  8 0 6  4 0 0

   0 0 8  1 0 2  9 0 0
   7 0 0  0 0 0  0 0 8
   0 0 6  7 0 8  2 0 0

   0 0 2  6 0 9  5 0 0
   8 0 0  2 0 3  0 0 9
   0 0 5  0 1 0  3 0 0])

(def b3
  [4 0 0  0 0 0  8 0 5
   0 3 0  0 0 0  0 0 0
   0 0 0  7 0 0  0 0 0
   0 2 0  0 0 0  0 6 0
   0 0 0  0 8 0  4 0 0
   0 0 0  0 1 0  0 0 0
   0 0 0  6 0 3  0 7 0
   5 0 0  2 0 0  0 0 0
   1 0 4  0 0 0  0 0 0])

(def empty-values
  (into {} (for [s squares] [s digits])))

;; consttraint propagation
;; 1) If a square has one possible value,
;;    then eliminate that value from its peers
;; 2) If a unit has one possible place for a value
;;    then assign that value to that place

(defn display
  "Display values as a 2D grid"
  [values]
  (let [width (inc (apply max (map (comp count values) squares)))
        line (string/join "+" (repeat 3 (string/join (repeat (* 3 width) "-"))))]
    (println)
    (println)
    (doseq [r rows]
      (println r " " (string/join
                      (for [c cols]
                        (format (str "%-" width "s%s")
                                (string/join (values (str r c)))
                                (if (#{"3" "6"} c) "| " "")))))
      (when (#{"C" "F"} r) (println "   " line)))))

(defn places
  "return the squares in given unit that contain given digit"
  [values unit digit]
  (for [square unit
        :when (and (values square)
                   (not= -1 (.indexOf (values square) digit)))]
    square))

(declare assign)

(defn eliminate
  "eliminate digit from square's values
  return nil if error is encountered
  otherwise return updaed values"
  [values square digit]
  (if (= -1 (.indexOf (values square) digit))
    values
    (let [sq-values (values square)
          remaining-digits (string/replace sq-values digit "")
          num-values (count remaining-digits)]
      (when (> num-values 0)
        (let [values (assoc values square remaining-digits)
              values (if (= num-values 1)
                       (loop [ps (peers square) new-values values]
                         (if-let [p (first ps)]
                           (when-let [updated-values
                                      (eliminate new-values p remaining-digits)]
                             (recur (rest ps) updated-values))
                           new-values))
                       values)]
          (let [us (units square)]
            (loop [us us values values]
              (when values
                (if-let [u (first us)]
                  (let [places (places values u digit)
                        cp (count places)]
                    (when (> cp 0)
                      (if (= 1 cp)
                        (recur (rest us) (assign values (first places) digit))
                        (recur (rest us) values))))
                  values)))))))))

(defn assign
  "eliminate all other values in square
  return nil if error is encountered
  other wise return updated values"
  [values square digit]
  (let [sq-values (values square)
        other-digits (string/replace sq-values digit "")]
    (loop [ds other-digits new-values values]
      (if-let [d (first ds)]
        (do
          (when-let [updated-values (eliminate new-values square d)]
              (recur (rest ds) updated-values)))
        new-values))))

(comment
  (require '[clojure.string :as string])
  ;; remove digit from value string
  (string/replace "123456789" "4" "") ;;=> "12356789"
  (some #{"4"} "123456789")
  ((set "12356789") "4")
  (not= -1 (.indexOf "123456789" "4"))
  (.-length "1")

  ;; JavaScript Regular Expressions
  ;; clojurescript regex literal
  ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
  ;; http://stackoverflow.com/questions/18735665/how-can-i-get-the-positions-of-regex-matches-in-clojurescript?rq=1

  (def a-regexp #"\d{3}-?\d{3}-?\d{4}")
  (.exec a-regexp "123-123-1234") ;;=> #js ["123-123-1234"]
  (.exec a-regexp "3a1b2c1d") ;;=> nil
  (.-source a-regexp) ;;=> "\\d{3}-?\\d{3}-?\\d{4}"
  )

(defn parse-grid
  "Convert grid to map with square keys
  and vals equal to all possible values
  of the square, represented as a string of digits
  return nil if error"
  [grid]
  (let [grid-string (map (fn [i] (if (= 0 i)
                                   nil
                                   (str i)))
                         grid)
        values empty-values
        bs (zipmap squares grid-string)]
    (loop [bs bs values values]
      (if-let [b (first bs)]
        (if (val b)
          (when-let [new-values (assign values (key b) (val b))]
            (recur (rest bs) new-values))
          (recur (rest bs) values))
        values))))

(defn search
  "Using depth-first search and propagation, try all possible values"
  [values]
  (when values
    (let [scount (comp count values)]
      (if (every? #(= 1 (scount %)) squares)
        values ;solved!
        (let [s (apply min-key scount (filter #(< 1 (scount %)) squares))]
          (some identity (for [d (values s)]
                           (search (assign values s d)))))))))

(defn solve [grid] (-> grid parse-grid search))

(comment
  (require '[om-svg-tut.sudoku :as s] :reload)

  (def values (s/parse-grid s/b1))
  (s/display values)

  (def values (s/parse-grid s/b2))
  (s/display values)

  (def values (s/parse-grid s/b3))
  (s/display values)
  (s/search values)

  (def m
    (into {}
          (for [s s/squares
                :let [c (values s)]
                :when (> (count c) 1)] [c s])))

  (m (apply min-key count (keys m)))

  (for [s s/squares :let [c (values s)] :when (< (count c) 1)] [s c])
  )
