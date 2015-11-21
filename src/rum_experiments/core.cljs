(ns rum-experiments.core
  (:require [goog.dom :as gdom]
            [rum-experiments.util :as util]
            [cats.labs.lens :as l]
            [sablono.core :as html :refer-macros [html]]
            [rum.core :as rum :include-macros true]))

(defonce state
  (atom {:title "hello world"
         :counter 0
         :items [{:id 0 :counter 1}
                 {:id 1 :counter 2}]}))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counter Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn counter-render
  [own]
  (let [item (get-in own [:rum/props :item])]
    ;; (println "counter-render" (:id @item))
    (html
     [:section {:class-name "counter" :style {:padding "5px"
                                              :margin "3px"
                                              :border "1px solid #efefef"}}
      [:div (str "Counter id=" (:id @item) " val=" (:counter @item))]
      [:div
       [:button {:on-click #(swap! item update :counter inc)}
        "click-me"]]])))

(def counter
  (util/component
   util/cursored
   {:render counter-render}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn root-render
  [own]
  (let [state (get-in own [:rum/props :state])]
    (html
     [:section
      [:section {:class-name "global"}
       [:div (str "Counter: " (:counter @state))]
       [:div
        [:button {:on-click #(swap! state update :counter inc)}
         "click me!"]]]
      [:section {:class-name "items" :style {:padding-top "20px"}}
     (for [item-data (:items state)]
       (-> (counter {:item item-data})
           (rum/with-key @(:id item-data))))]])))

(def root
  (util/component
   util/cursored
   {:render root-render}))

(rum/mount (root {:state (util/focus state)})
           (gdom/getElement "app"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defmulti read-fn (fn [k p] k))
  (defmulti novelty-fn (fn [k p] k))

  (defmethod read-fn :counters
    [_ params]
    (letfn [(index-counter [state item]
              (assoc-in [:counters-by-id (:id item)] item))
            (index-counters [state items]
              (reduce index-counters state items))
            (add-counters [state items]
              (assoc state :counters items))]
      (m/mlet [items (api/get-counters)]
        (m/return
         {:action (fn [state]
                    (-> state
                        (add-counters items)
                        (index-counters items)))}))))

  (defmethod read-fn :title
    [_ params]
    )

  (defmethod novelty-fn :increment
    [_ {:keys [id] :as params}]
    {:action #(update-in % [:couters id] inc)})

  (defn init
    [s]
    ;; The emit! function should
    ;; return a promise that will
    ;; be resolved when all operations
    ;; are completely done
    (st/emit! s [[:read :counters]
                 [:read :title]]))

  (def store
    (st/store {:state state
               :init init
               :read read-fn
               :novelty novelty-fn}))

  ;; Rum components

  (defn counter-render
    [own]
    (let [item (get-in own [:rum/props :item])
          on-click #(st/novelty! store [:increment {:id (:id @item)}])]

      (html
       [:div (str "Counter id=" (:id @item) " val=" (:counter @item))]
       [:button {:on-click on-click} "click-me"])))

  (defn root-render
    [own]
    (let [state (get-in own [:rum/props :state])]
      (html
       [:section {:class-name "counters"}
        (for [item (:counters @state)]
          (let [item (util/derive state [:counters-by-id (:id item)])]
            (-> (counter {:item item})
                (rum/with-key @(:id item-data)))))])))


  (def counter
    (util/component util/cursored {:render counter-render}))

  (def root
    (util/component util/cursored {:render root-render}))

  (rum/mount (root {:state (util/focus state)})
             (gdom/getElement "app"))
  )
