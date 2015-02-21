(ns om-svg-tut.board
  (:require [clojure.set :as set]))

(def sudoku-board
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]])

;; values
;; 0 represents no value
(def zero #{0})

;; values are represented by integers 1 .. 9
(def value-set #{1 2 3 4 5 6 7 8 9})

;; positions from 0 to 8
(def position-set (set (range 9)))

(def complete-set (set/union zero value-set))

(defn valid-value?
  "checks if p is a valid value, includes 0, for no value"
  [p]
  (complete-set p))

;; sudoku board
;; a board is a vector of 9 rows
;; each row has value in complete-set

;; an emptry row in a vector with nine zeros
(def empty-row (vec (repeat 9 0)))

;; an empty board is a vector of
;; 9 empty-rows
(defn empty-board []
  (vec (repeat 9 empty-row)))

;; a position is a vector of int
;; where x,y in [0..9]
;; [0 0] through [8 8]
(defn valid-position?
  "is the position with bounds?"
  [position]
  (let [[x y] position]
    (and (position-set x) (position-set y))))

;; accesser methods
;; position
(defn value
  "return value of board at position"
  [board position]
  (get-in board position))

;; row col box
(defn row
  "return full row for given row-num"
  [board row-num]
  (assert (position-set row-num))
  (board row-num))

(defn col
  "return full column for given column number"
  [board col-num]
  (mapv
   (fn [i] (value board [i col-num]))
   (range 9)))

(defn box-num->start-pos
  "return start position for given box number"
  [box-num]
  (let [row-start (cond
                    (< box-num 3) 0
                    (< box-num 6) 3
                    :else   6)
        col-start (let [q (mod box-num 3)]
                    (cond
                      (= q 0) 0
                      (= q 1) 3
                      (= q 2) 6))]
    [row-start col-start]))

(defn pos->box-num
  "return box number for given position"
  [pos]
  (let [[x y] pos
        xf (cond (< x 3) 0
                 (< x 6) 1
                 :else 2)
        yf (cond (< y 3) 0
                 (< y 6) 3
                 :else 6)]
    (+ yf xf)))

(defn pos->start-pos
  "return start position of box for given [row col]"
  [pos]
  (box-num->start-pos (pos->box-num pos)))

(defn box
  "return full box for given box number"
  [board box-num]
  (let [[i j] (box-num->start-pos box-num)]
    (for [x (range i (+ 3 i))
          y (range j (+ 3 j))]
      (value board [x y]))))

(defn row-values
  "return values in board at row"
  [board row-num]
  (set (row board row-num)))

(defn col-values
  "return values in board at col"
  [board col-num]
  (set (col board col-num)))

(defn box-values
  "return values in box determined from pos"
  [board box-num]
  (set (box board box-num)))

(defn valid-move?
  "is the move valid?
there can not be any other object in the same
  row, column or box with the given value"
  [board position value]
  (let [[row col] position
        row-vals (row-values board row)
        col-vals (col-values board col)
        box-vals (box-values board position)]
    (not (or (row-vals value)
             (col-vals value)
             (box-vals value)))))

(defn move
  "update board to value at position"
  [board position value]
  (assoc-in board position value))

(defn positions-for-value
  "return a sequence of all locations of given value
in given board"
  [board target]
  (for [i (range 9)
        j (range 9)
        :let [value (get-in board [i j])]
        :when (= value target)]
    [i j]))
