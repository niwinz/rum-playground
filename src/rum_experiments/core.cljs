(ns rum-experiments.core
  (:require [goog.dom :as gdom]
            [rum-experiments.util :as util]
            [rum-experiments.state :as s]
            [promesa.core :as p]
            [cats.labs.lens :as l]
            [cats.core :as m]
            [sablono.core :as html :refer-macros [html]]
            [rum.core :as rum :include-macros true]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce state (atom {}))

(defmulti state-transition
  (fn [_ [event]] event))

(defmethod state-transition :index-counters
  [state [_ counters]]
  (letfn [(index-counter [state item]
            (assoc-in state [:counters-by-id (:id item)] item))]
    (as-> (assoc state :counter-ids (mapv :id counters)) state
      (reduce index-counter state counters))))

(defmethod state-transition :set-counters
  [state [_ counters]]
  (-> (assoc state :counters counters)
      (state-transition [:index-counters counters])))

(defmethod state-transition :add
  [state [_ id]]
  (let [item {:id (str (random-uuid)) :num 0}
        counters (:counters state)]
    (-> (assoc state :counters (conj counters item))
        (state-transition [:index-counters counters]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client-Server Arch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti read-fn (fn [s k p] k))
(defmulti novelty-fn (fn [s k p] k))

(defmethod read-fn :load-counters
  [_ _ num]
  ;; Simulate backend query with a little lag
  ;; TODO: read it from database
  (m/mlet [_ (p/delay 500)]
    (m/return {:items []})))

(defmethod novelty-fn :toggle-state
  [store key id]
  ;; TODO: persist it in a database
  (swap! store state-transition [:toggle-state id]))

(defmethod novelty-fn :add
  [store key _]
  (swap! store state-transition [:add]))

(defmethod novelty-fn :delete
  [store key id]
  (swap! store state-transition [:delete id]))

(defn init-fn
  [store]
  (m/mlet [p (s/read store [:load-counters])]
    (swap! store state-transition [:set-counters (:items p)])
    (println "Initialized")))

(def store
  (s/store state {:init init-fn
                  :read read-fn
                  :novelty novelty-fn}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counter Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn counter-render
  [own]
  (let [item (get-in own [:rum/props :item])]
    (html
     [:li {:class-name "item noselect"}
      [:button {:on-click #(s/novelty store [:delete (:id @item)])} "x"]
      [:span {:on-click #(s/novelty store [:increment (:id @item)])
              :style {:font-style "monospace"}}
       (str " counter (" (:id @item) ") => " (:num @item))]])))

(def todo-item
  (util/component
   {:name "counter"
    :mixins [util/cursored]
    :render counter-render}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn root-render
  [own]
  (let [state (get-in own [:rum/props :state])]
    (html
     [:section {:class-name "root"}
      [:div {:class-name "title"} "Counters:"]
      [:ul {:class-name "items"}
       (for [cid (:counter-ids @state)]
         (let [item (util/derive state [:counters-by-id cid])
               cnt (todo-item {:item item})]
            (rum/with-key cnt cid)))]
      [:button {:on-click #(s/novelty store [:add])} "+"]]
     )))

(def root
  (util/component
   {:render root-render
    :name "root"
    :mixins [util/cursored]}))

(let [state (util/focus state)]
  (rum/mount (root {:state state})
             (gdom/getElement "app")))

