(ns rum-experiments.state
  (:require [promesa.core :as p]))

;; (defprotocol IResponseHandler
;;   (-read-result [] ))

;; (defprotocol IStore
;;   (-init [_] "Initialize the store")
;;   (-read [_

(defrecord Store [state init read novelty]
  cljs.core/IDeref
  (-deref [_] state)

  cljs.core/IWatchable
  (-add-watch [self key cb]
    (add-watch state key (fn [key _ oldval newval]
                          (if (not= oldval newval)
                            (cb key self oldval newval)))))
  (-remove-watch [_ key]
    (remove-watch state key))

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

  IStore
  (-emit [_ type key params]
    (case type
      :read (read

(defn store
  [{:keys [state init read novelty] :as opts}]
  {:pre [(fn? read) (fn? novelty)
         (instance cljs.core.Atom state)]}
  (let [store (Store. state read novelty)]
    (-init store)
    store))

(defn- interpret-params
  [params]
  (cond
    (and (vector? params)
         (every? vector? params)
         (every? keyword? (map first params)))
    params

    (and (vector? params)
         (keyword? (first params)))


(defn read
  [store params]
  (let [result (-emit store :read k params)]
    (cond
      (p/promise? result)
      (p/then result (fn [result]
                       (if (fn? result)
                         (swap! (.-state store) result)
                          result)))

       (fn? result)
       (do
         (swap! (.-state store) result)
         (p/resolved nil))

       :else
       (p/resolved result)))))

(defn novelty
  ([store k]
   (novelty store k nil))
  ([store k params]
   (let [result (-novelty store k params)]
     (cond
       (p/promise? result)
       (p/then result (fn [result]
                        (if (fn? result)
                          (swap! (.-state store) result)
                          result)))

       (fn? result)
       (do
         (swap! (.-state store) result)
         (p/resolved nil))

       :else
       (p/resolved result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Local state management
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The global state transtions should be predecible
  ;; and testable and this way offers to have flexible
  ;; and extensible way to handle state transitions.

  (defmulti state-transition
    (fn [_ [event]] event))

  (defmethod state-transition :index-counters
    [state [_ counters]]
    (letfn [(index-counter [state item]
              (assoc-in [:counters-by-id (:id item)] item))]
      (reduce index-counters state counters)))

  (defmethod state-transition :add-counters
    [state [_ counters]]
    (let [counter-ids (mapv :id counters)]
      (-> (assoc state :counters counter-ids)
          (state-transition [:index-counters data]))))

  ;; Example how to be used in a single operation
  ;; in a "transaction" over state
  (swap! state state-transition [:add-counters foobar])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Client Server Architecture
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The client-server architecture is suitable for build
  ;; client-server communications or also client only architectures
  ;; where requires communication with some local databases
  ;; such as datascript, localstorage or indexeddb...
  ;;
  ;; It consists in two main operations: read that consists
  ;; for retrieve data from the "database" and novelty for
  ;; mutate the persistent layer sotorage state.

  (defmulti read-fn (fn [k p] k))
  (defmulti novelty-fn (fn [k p] k))

  ;; The return value of that functions can be plain value or promise
  ;; instance (internally all is coerced to promise for uniform api).
  ;; The value can be any object or function. If function is provided,
  ;; it will be automatically executed in context of the state value
  ;; (just like executing it with swap!).

  ;; This is completly agnostic of the persistence layer so you
  ;; can use whatever you want for access the data. The good examples
  ;; are use funcool/muse and datascript.

  (defmethod read-fn :counters
    [_ params]
    ;; Here you can use any method for obtain date such as using
    ;; traditional api rest, postal, websockets, whatever.
    (m/alet [items (api/get-counters)]
      #(state-transition % [:add-counters items])))

  (defmethod novelty-fn :increment
    [_ {:keys [id] :as params}]
    #(state-transition % [:increment id]))

  ;; The init function is some kind of entry point of the storage
  ;; and it will be executed as soon as posible. It is responsible
  ;; to populate some initial state.

  (defn init
    [s]
    (m/mlet [p (r/read! store [:counters :useless-param])]
      (println "Initialized")))

  ;; The store should be created with

  (def store
    (st/store {:state state
               :init init
               :read read-fn
               :novelty novelty-fn}))

  )
