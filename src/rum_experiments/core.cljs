(ns rum-experiments.core
  (:require [goog.dom :as gdom]
            [hodgepodge.core :refer [local-storage]]
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
        counters (conj (:counters state) item)]
    (-> (assoc state :counters counters)
        (state-transition [:index-counters counters]))))

(defmethod state-transition :increment
  [state [_ id]]
  (update-in state [:counters-by-id id :num] inc))

(defmethod state-transition :decrement
  [state [_ id]]
  (update-in state [:counters-by-id id :num] dec))

(defmethod state-transition :delete
  [state [_ id]]
  (let [counters (filter #(not= (:id %) id) (:counters state))]
    (-> (assoc state :counters counters)
        (state-transition [:index-counters counters]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client-Server Arch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti read-fn (fn [s k p] k))
(defmulti novelty-fn (fn [s k p] k))

(defmethod read-fn :load-counters
  [_ _ num]
  (get local-storage :counters {:items []}))

(defmethod novelty-fn :default
  [store key param]
  (swap! store state-transition [key param]))

(defmethod novelty-fn :save
  [store key param]
  (let [data (mapv (fn [id]

                     (get-in @store [:counters-by-id id]))
                   (:counter-ids @store))]
    (assoc! local-storage :counters {:items data})))

(defn init-fn
  [store]
  (m/mlet [p (s/read store [:load-counters])]
    (swap! store state-transition [:set-counters (:items p)])
    (println "Initialized")))

(defonce store
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
      [:button {:on-click #(s/novelty store [:increment (:id @item)])} "+"]
      [:button {:on-click #(s/novelty store [:decrement (:id @item)])} "-"]
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
      [:button {:class-name "new-button"
                :on-click #(s/novelty store [:add])} "new"]
      [:button {:class-name "save-button" :style {:margin-left "2px"}
                :on-click #(s/novelty store [:save])} "save"]
      [:ul {:class-name "items"}
       (for [cid (:counter-ids @state)]
         (let [item (util/derive state [:counters-by-id cid])
               cnt (todo-item {:item item})]
            (rum/with-key cnt cid)))]]
     )))

(def root
  (util/component
   {:render root-render
    :name "root"
    :mixins [util/cursored]}))

(let [state (util/focus state)]
  (rum/mount (root {:state state})
             (gdom/getElement "app")))

