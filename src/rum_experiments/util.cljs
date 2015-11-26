(ns rum-experiments.util
  (:refer-clojure :exclude [derive])
  (:require [rum.core :as rum]
            [cats.labs.lens :as l]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursored & Lenses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private
  deref-map-xform
  (map (fn [[k v]] [k (if (satisfies? IDeref v) @v v)])))

(defn- cursored-key [state]
  (str ":rum/cursored-" (:rum/id state)))

(defn- deref-props [data]
  (into {} deref-map-xform data))

(def cursored
  {:did-mount
   (fn [state]
     (doseq [[k v] (:rum/props state)
             :when (satisfies? IWatchable v)]
       (add-watch v (cursored-key state)
                  (fn [_ _ _ _]
                    (rum/request-render (:rum/react-component state)))))
     state)

   :will-unmount
   (fn [state]
     (doseq [[k v] (:rum/props state)
             :when (satisfies? IWatchable v)]
       (remove-watch v (cursored-key state)))
     state)

    :transfer-state
    (fn [old new]
      (assoc new :rum/old-props (:rum/old-props old)))

    :should-update
    (fn [old-state new-state]
      (not= (:rum/old-props old-state) (deref-props (:rum/props new-state))))

    :wrap-render
   (fn [render-fn]
     (fn [state]
       (let [[dom next-state] (render-fn state)]
         [dom (assoc next-state :rum/old-props (deref-props (:rum/props state)))])))

   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lenses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn derive
  [a path]
  (l/focus-atom (l/in path) a))

(defn focus
  ([state]
   (l/focus-atom l/id state))
  ([lens state]
   (l/focus-atom lens state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for define components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn component
  [spec]
  (let [name (or (:name spec)
                 (str (gensym "rum-")))
        mixins (or (:mixins spec)
                   [])
        spec (merge (dissoc spec :name :mixins)
                    (when-let [rfn (:render spec)]
                      {:render #(vector (rfn %) %)}))
        cls (rum/build-class (conj mixins spec) name)
        ctr (fn self
              ([] (self {}))
              ([props]
               (let [state {:rum/props props}]
               (rum/element cls state nil))))]
    (with-meta ctr {:rum/class cls})))


