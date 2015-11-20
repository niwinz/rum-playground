(ns rum-experiments.util
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

;; This is similar Focus type as in cats.labs.lens with additional
;; features such as:
;; - it implements ISeqable, IIndexed and ILookup for derive other focuses.
;; - it does not trigger watchers if value does not changes.

(deftype Focus [lens a]
  IDeref
  (-deref [_] (l/focus lens @a))

  IWatchable
  (-add-watch [self key cb]
    (add-watch a key (fn [key _ oldval newval]
                       (let [old' (l/focus lens oldval)
                             new' (l/focus lens newval)]
                         (if (not= old' new')
                           (cb key self old' new'))))))
  (-remove-watch [_ key]
    (remove-watch a key))

  ISeqable
  (-seq [coll]
    (let [cnt (count coll)]
      (map #(Focus. (l/nth %) coll) (range cnt))))

  ICounted
  (-count [coll]
    (cljs.core/-count @coll))

  IIndexed
  (-nth [coll n]
    (cljs.core/-nth coll n nil))
  (-nth [coll n not-found]
    (Focus. (l/nth n) coll))

  ILookup
  (-lookup [coll k]
    (cljs.core/-lookup coll k nil))

  (-lookup [coll k not-found]
    (if (number? k)
      (Focus. (l/nth k) coll)
      (Focus. (l/key k) coll)))

  IReset
  (-reset! [self newval]
    (swap! a #(l/put lens newval %))
    (deref self))

  ISwap
  (-swap! [self f]
    (swap! a (fn [s] (l/over lens f s)))
    (deref self))

  (-swap! [self f x]
    (swap! a (fn [s] (l/over lens #(f % x) s)))
    (deref self))

  (-swap! [self f x y]
    (swap! a (fn [s] (l/over lens #(f % x y) s)))
    (deref self))

  (-swap! [self f x y more]
    (swap! a (fn [s] (l/over lens #(apply f % x y more) s)))
    (deref self)))

(defn focus-atom
  ([a]
   (focus-atom l/id a))
  ([lens a]
   (Focus. lens a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for define components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private
  render-xform
  (map (fn [item]
         (if (:render item)
           (let [rfn (:render item)]
             (assoc item :render (fn [state] [(rfn state) state])))
           item))))

(defn- component*
  [name specs]
  (let [cls (rum/build-class specs name)
        ctr (fn self
              ([] (self {}))
              ([props]
               (let [state {:rum/props props}]
               (rum/element cls state nil))))]
    (with-meta ctr {:rum/class cls})))

(defn component
  [& specs]
  (let [[name specs] (if (string? (first specs))
                       [(first specs) (rest specs)]
                       [(str (gensym "rum-")) specs])
        specs (into [] render-xform specs)]
    (component* name specs)))
