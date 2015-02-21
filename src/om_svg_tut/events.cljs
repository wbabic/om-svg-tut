(ns om-svg-tut.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan]]
            [goog.dom :as gdom]
            [goog.events :as events]))

;; mouse event utils

(def event-map
  {:mouse-down goog.events.EventType.MOUSEDOWN
   :mouse-move goog.events.EventType.MOUSEMOVE
   :mouse-up goog.events.EventType.MOUSEUP
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :key-down goog.events.EventType.KEYDOWN})

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type #(put! out %))
    out))

(defn mouse-chan [element event-type key]
  (async/map
   (fn [e] (let [px (.-offsetX e)
                 py (.-offsetY e)
                 _ (.log js/console e)
                 ]
             [key [px py]]))
   [(listen element (event-type event-map))]))

(defn events->chan
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
     (events/listen el (event-type event-map)
                    (fn [e] (put! c e)))
     c))

(defn keys-chan []
  (events->chan js/window :key-down
                (chan 1 (comp (map #(.-keyCode %))
                              (filter #{37 38 39 40
                                        48 49 50 51 52 53 54 55 56 57 78 80})
                              (map {78 [:next-object]
                                    80 [:prev-object]
                                    37 [:move :left]
                                    38 [:move :up]
                                    39 [:move :right]
                                    40 [:move :down]
                                    48 [:value 0]
                                    49 [:value 1]
                                    50 [:value 2]
                                    51 [:value 3]
                                    52 [:value 4]
                                    53 [:value 5]
                                    54 [:value 6]
                                    55 [:value 7]
                                    56 [:value 8]
                                    57 [:value 9]})))))
